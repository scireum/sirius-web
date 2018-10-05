/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.http;

import com.google.common.collect.Maps;
import io.netty.bootstrap.ServerBootstrap;
import io.netty.buffer.PooledByteBufAllocator;
import io.netty.channel.Channel;
import io.netty.channel.ChannelHandlerContext;
import io.netty.channel.ChannelInitializer;
import io.netty.channel.ChannelOption;
import io.netty.channel.EventLoopGroup;
import io.netty.channel.WriteBufferWaterMark;
import io.netty.channel.nio.NioEventLoopGroup;
import io.netty.channel.socket.SocketChannel;
import io.netty.channel.socket.nio.NioServerSocketChannel;
import io.netty.handler.codec.http.HttpRequest;
import io.netty.handler.codec.http.multipart.Attribute;
import io.netty.handler.codec.http.multipart.DefaultHttpDataFactory;
import io.netty.handler.codec.http.multipart.DiskAttribute;
import io.netty.handler.codec.http.multipart.DiskFileUpload;
import io.netty.handler.codec.http.multipart.HttpDataFactory;
import io.netty.util.ResourceLeakDetector;
import sirius.kernel.Killable;
import sirius.kernel.Sirius;
import sirius.kernel.Startable;
import sirius.kernel.Stoppable;
import sirius.kernel.async.Operation;
import sirius.kernel.commons.Strings;
import sirius.kernel.commons.Tuple;
import sirius.kernel.commons.Value;
import sirius.kernel.di.GlobalContext;
import sirius.kernel.di.std.ConfigValue;
import sirius.kernel.di.std.Part;
import sirius.kernel.di.std.Register;
import sirius.kernel.health.Average;
import sirius.kernel.health.Exceptions;
import sirius.kernel.health.Log;
import sirius.kernel.health.metrics.MetricProvider;
import sirius.kernel.health.metrics.MetricsCollector;
import sirius.kernel.timer.EveryTenSeconds;

import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.UnknownHostException;
import java.time.Duration;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

/**
 * Responsible for setting up and starting netty as HTTP server.
 */
@Register(framework = "web.http")
public class WebServer implements Startable, Stoppable, Killable, MetricProvider {

    /**
     * Determines the priority of the start of the web server. This is exposed as public so that other life cycles
     * can determine their own priority on this.
     */
    public static final int LIFECYCLE_PRIORITY = 500;

    /**
     * Used to log all web / http relevant messages
     */
    public static final Log LOG = Log.get("web");

    /**
     * Config value of the HTTP port used (<tt>http.port</tt>). The default port for HTTP is 80, but the default value
     * here is 9000 because non root users cannot open ports less than 1024
     */
    @ConfigValue("http.port")
    private static int port;

    /**
     * Config value for additional ports (next to the default port). This is only used in edge cases and by default
     * turned off. Add additional ports by naming them under <tt>http.additionalPorts</tt> in the system config.
     */
    @ConfigValue("http.additionalPorts")
    private static List<String> additionalPorts;

    /**
     * Config value of the HTTP bind address used (<tt>http.bindAddress</tt>). If the value is empty, we bind all
     * addresses. Otherwise this can be used to bind against a single IP address in order to setup multiple servers
     * on the same port.
     */
    @ConfigValue("http.bindAddress")
    private String bindAddress;

    /**
     * Config value of the max size for uploads which are kept entirely in memory (<tt>"http.uploadDiskThreshold</tt>).
     */
    @ConfigValue("http.uploadDiskThreshold")
    private static long uploadDiskThreshold;

    /**
     * Config value of the min free space on disk (<tt>"http.minUploadFreespace</tt>). If the free space on disk drops
     * below this value, we cancel an upload. This provides large uploads which never crash a system by jamming a
     * disk.
     */
    @ConfigValue("http.minUploadFreespace")
    private static long minUploadFreespace;

    /**
     * Config value of the max upload size (<tt>http.maxUploadSize</tt>). Can be used to specify an upper limit for
     * file uploads.
     */
    @ConfigValue("http.maxUploadSize")
    private static long maxUploadSize;

    /**
     * Config value of the maximal tolerated time it takes to generate a response. (<tt>http.maxTimeToFirstByte</tt>).
     * Requests which are not marked as <tt>long running</tt> but take longer, will be logged.
     */
    @ConfigValue("http.maxTimeToFirstByte")
    private static long maxTimeToFirstByte;

    /**
     * Contains a list of IP ranges which are permitted to access this server. Access from unauthorized IPs will be
     * blocked at the lowest level possible (probably no connection will be accepted). The format accepted by this
     * field is defined by {@link IPRange#paraseRangeSet(String)}.
     *
     * @see IPRange#paraseRangeSet(String)
     */
    @ConfigValue("http.firewall.filterIPs")
    private static String ipFilter;
    private static IPRange.RangeSet filterRanges;
    private Channel channel;
    private Channel sslChannel;

    @ConfigValue("http.ssl.enabled")
    private boolean ssl;

    @ConfigValue("http.ssl.port")
    private int sslPort;

    @ConfigValue("http.firewall.proxyIPs")
    private static String proxyIPs;
    private static IPRange.RangeSet proxyRanges;

    @Part
    private GlobalContext ctx;

    private static HttpDataFactory httpDataFactory;

    private static final String UNKNOWN_FORWARDED_FOR_HOST = "unknown";
    private static final String HEADER_X_FORWARDED_FOR = "X-Forwarded-For";

    /**
     * Indicates that netty itself will compute the optimal number of threads in the event loop
     */
    private static final int AUTOSELECT_EVENT_LOOP_SIZE = 0;
    private EventLoopGroup eventLoop;

    protected static AtomicLong bytesIn = new AtomicLong();
    protected static AtomicLong bytesOut = new AtomicLong();
    protected static AtomicLong messagesIn = new AtomicLong();
    protected static AtomicLong messagesOut = new AtomicLong();
    protected static AtomicLong connections = new AtomicLong();
    protected static AtomicLong blocks = new AtomicLong();
    protected static AtomicLong requests = new AtomicLong();
    protected static AtomicLong chunks = new AtomicLong();
    protected static AtomicLong keepalives = new AtomicLong();
    protected static AtomicLong idleTimeouts = new AtomicLong();
    protected static AtomicLong clientErrors = new AtomicLong();
    protected static AtomicLong serverErrors = new AtomicLong();
    protected static AtomicLong websockets = new AtomicLong();
    protected static Map<WebServerHandler, ActiveHTTPConnection> openConnections = Maps.newConcurrentMap();
    protected static Average responseTime = new Average();
    protected static Average timeToFirstByte = new Average();
    protected static Average queueTime = new Average();
    protected static volatile MicrotimingMode microtimingMode = MicrotimingMode.URI;

    /**
     * Returns the port used by the web server
     *
     * @return the port served by the HTTP server
     */
    public static int getPort() {
        return port;
    }

    /**
     * Returns an ip filter which determines which IPs may connect to the web server.
     *
     * @return a range set describing all accepted ranges
     */
    protected static IPRange.RangeSet getIPFilter() {
        if (filterRanges == null) {
            try {
                filterRanges = IPRange.paraseRangeSet(ipFilter);
            } catch (Exception e) {
                Exceptions.handle()
                          .to(LOG)
                          .error(e)
                          .withSystemErrorMessage(
                                  "Error parsing config value: 'http.firewall.filterIPs': %s (%s). Defaulting to localhost!")
                          .handle();
                filterRanges = IPRange.LOCALHOST;
            }
        }

        return filterRanges;
    }

    /**
     * Returns all proxy IPs as {@link IPRange.RangeSet}
     *
     * @return a range set describing all proxy ranges. This is probably just a single IP addressed, however, using
     * {@link IPRange.RangeSet} permit to name several, even sub nets. If a request comes from a proxy IP
     * its X-Forwarded-For header is checked for the original IP.
     */
    protected static IPRange.RangeSet getProxyIPs() {
        if (proxyRanges == null) {
            try {
                proxyRanges = IPRange.paraseRangeSet(proxyIPs);
            } catch (Exception e) {
                Exceptions.handle()
                          .to(LOG)
                          .error(e)
                          .withSystemErrorMessage("Error parsing config value: 'http.firewall.proxyIPs': %s (%s)")
                          .handle();
                proxyRanges = IPRange.NO_FILTER;
            }
        }

        return proxyRanges;
    }

    /**
     * Tries to determine the effective remote IP for the given context and request.
     * <p>
     * This is the remote address of the channel. However, if recognized as a proxy,
     * we use the last IP address given in the X-Forarded-For header.
     *
     * @param ctx     the channel context used to determine the physical IP
     * @param request the request used to read the appropriate headers for reverse proxies
     * @return the effective remote address of the client
     */
    public static InetAddress determineRemoteIP(ChannelHandlerContext ctx, HttpRequest request) {
        if (ctx == null || request == null) {
            try {
                return InetAddress.getLocalHost();
            } catch (UnknownHostException e) {
                throw Exceptions.handle(e);
            }
        }

        InetAddress ip = ((InetSocketAddress) ctx.channel().remoteAddress()).getAddress();
        return applyProxyIPs(ip, request);
    }

    private static InetAddress applyProxyIPs(InetAddress ip, HttpRequest request) {
        if (WebServer.getProxyIPs().isEmpty()) {
            return ip;
        }

        if (!WebServer.getProxyIPs().accepts(ip)) {
            return ip;
        }

        Value forwardedFor = Value.of(request.headers().get(HEADER_X_FORWARDED_FOR));
        if (!forwardedFor.isFilled()) {
            return ip;
        }

        try {
            // A X-Forwarded-For might contain many IPs like 1.2.3.4, 5.6.7.8... We're only interested
            // in the last IP -> cut appropriately
            Tuple<String, String> splitIPs = Strings.splitAtLast(forwardedFor.asString(), ",");
            String forwardedForIp =
                    Strings.isFilled(splitIPs.getSecond()) ? splitIPs.getSecond().trim() : splitIPs.getFirst().trim();

            // Some reverse proxies like pound use this value if an invalid X-Forwarded-For is given
            if (UNKNOWN_FORWARDED_FOR_HOST.equals(forwardedForIp)) {
                return ip;
            }

            return InetAddress.getByName(forwardedForIp);
        } catch (Exception e) {
            Exceptions.ignore(e);
            WebServer.LOG.WARN(Strings.apply(
                    "Cannot parse X-Forwarded-For address: %s, Remote-IP: %s, Request: %s - %s (%s)",
                    forwardedFor,
                    ip,
                    request.uri(),
                    e.getMessage(),
                    e.getClass().getName()));
            return ip;
        }
    }

    /**
     * Determines how the web server should participate in the microtiming framework.
     */
    public enum MicrotimingMode {
        /**
         * Use the remote ip as key - this can be used to group requests by remote ip
         */
        IP,

        /**
         * Use the requested uri as key - this can be used to group requests by uri
         */
        URI,

        /**
         * Use remote ip and uri as key - this can be used to lear which peer accesses which uris frequently
         */
        BOTH;

        /*
         * Computes the key used for microtiming based on the current mode
         */
        protected String getMicrotimingKey(WebContext context) {
            switch (this) {
                case IP:
                    return context.getRemoteIP().toString();
                case BOTH:
                    return context.getRemoteIP().toString() + " <-- " + context.microtimingKey;
                default:
                    return context.microtimingKey;
            }
        }
    }

    /**
     * Returns the minimal value of free disk space accepted until an upload is aborted.
     *
     * @return the minimal allowed free disk space in bytes
     */
    protected static long getMinUploadFreespace() {
        return minUploadFreespace;
    }

    /**
     * Returns the maximal upload size in bytes
     *
     * @return the max size of uploaded files
     */
    protected static long getMaxUploadSize() {
        return maxUploadSize;
    }

    /**
     * Returns the maximal tolerated time it takes to create a response (send the first byte).
     *
     * @return the maximal time to first byte in millis
     */
    protected static long getMaxTimeToFirstByte() {
        return maxTimeToFirstByte;
    }

    /**
     * Returns the data factory used to handle file uploads and posts.
     *
     * @return the data factory used for processing POST and PUT requests.
     */
    protected static HttpDataFactory getHttpDataFactory() {
        return httpDataFactory;
    }

    @Override
    public int getPriority() {
        return LIFECYCLE_PRIORITY;
    }

    @Override
    public void started() {
        if (port <= 0) {
            LOG.INFO("web server is disabled (http.port is <= 0)");
            return;
        }
        reportSettings();
        configureNetty();
        try (Operation op = new Operation(() -> "WebServer.createHTTPChannel:" + port, Duration.ofSeconds(15))) {
            createHTTPChannel(port);
        }
        for (String additionalPort : additionalPorts) {
            Value additionalPortValue = Value.of(additionalPort);
            if (additionalPortValue.isNumeric()) {
                try (Operation op = new Operation(() -> "WebServer.createHTTPChannel:" + additionalPort,
                                                  Duration.ofSeconds(15))) {
                    createHTTPChannel(additionalPortValue.asInt(-1));
                }
            }
        }

        if (ssl) {
            try (Operation op = new Operation(() -> "WebServer.createHTTPSChannel", Duration.ofSeconds(15))) {
                createHTTPSChannel();
            }
        }
    }

    private void reportSettings() {
        LOG.INFO("Initializing netty at port %d", port);

        for (String additionalPort : additionalPorts) {
            LOG.INFO("Initializing netty also at port %s", additionalPort);
        }

        if (Strings.isFilled(bindAddress)) {
            LOG.INFO("Binding netty to %s", bindAddress);
        }
        if (ssl) {
            LOG.INFO("Starting SSL on port %d", sslPort);
        }

        if (Sirius.isDev() && !Sirius.getSettings().getConfig().hasPath("http.noLeakDetection")) {
            ResourceLeakDetector.setLevel(ResourceLeakDetector.Level.PARANOID);
            LOG.INFO("Enabling PARANOID resource leak detection...");
        }
    }

    /**
     * As some client (namely ICEFaces) tend to create POSTs which contain empty
     * field names (which is forbidden by nettys implementation), we resort to
     * use "unknown" as backup name for those values so that the requests can
     * be processes without failing.
     */
    private static class SiriusHttpDataFactory extends DefaultHttpDataFactory {

        SiriusHttpDataFactory(long minSize) {
            super(minSize);
        }

        @Override
        public Attribute createAttribute(HttpRequest request, String name) {
            return super.createAttribute(request, Strings.isEmpty(name) ? "_sirius_unknown" : name);
        }

        @Override
        public Attribute createAttribute(HttpRequest request, String name, String value) {
            return super.createAttribute(request, Strings.isEmpty(name) ? "_sirius_unknown" : name, value);
        }
    }

    private void configureNetty() {
        setupUploads();
        try (Operation op = new Operation(() -> "WebServer.createEventLoop", Duration.ofSeconds(15))) {
            eventLoop = createEventLoop(AUTOSELECT_EVENT_LOOP_SIZE, "netty-");
        }
    }

    private static void setupUploads() {
        DiskFileUpload.deleteOnExitTemporaryFile = true;
        DiskFileUpload.baseDirectory = null;
        DiskAttribute.deleteOnExitTemporaryFile = true;
        DiskAttribute.baseDirectory = null;
        httpDataFactory = new SiriusHttpDataFactory(uploadDiskThreshold);
    }

    private static class PrefixThreadFactory implements ThreadFactory {
        private final String name;
        private final AtomicInteger counter = new AtomicInteger(1);

        private PrefixThreadFactory(String name) {
            this.name = name;
        }

        @Override
        public Thread newThread(Runnable r) {
            return new Thread(r, name + counter.getAndIncrement());
        }
    }

    private EventLoopGroup createEventLoop(int numThreads, String name) {
        return new NioEventLoopGroup(numThreads, new PrefixThreadFactory(name));
    }

    private ServerBootstrap createServerBootstrap(ChannelInitializer<SocketChannel> initializer) {
        ServerBootstrap bootstrap = new ServerBootstrap();
        bootstrap.childOption(ChannelOption.WRITE_BUFFER_WATER_MARK, WriteBufferWaterMark.DEFAULT);
        bootstrap.childOption(ChannelOption.ALLOCATOR, PooledByteBufAllocator.DEFAULT);
        // At mose have 128 connections waiting to be "connected" - drop everything else...
        bootstrap.option(ChannelOption.SO_BACKLOG, 128);
        // Send a KEEPALIVE packet every 2h and expect and ACK on the TCP layer
        bootstrap.childOption(ChannelOption.SO_KEEPALIVE, true);
        bootstrap.group(eventLoop);
        bootstrap.channel(NioServerSocketChannel.class);
        bootstrap.childHandler(ctx.wire(initializer));
        return bootstrap;
    }

    private void createHTTPChannel(int port) {
        try {
            ServerBootstrap bootstrap = createServerBootstrap(new WebServerInitializer());
            // Bind and start to accept incoming connections.
            if (Strings.isFilled(bindAddress)) {
                channel = bootstrap.bind(new InetSocketAddress(bindAddress, port)).sync().channel();
            } else {
                channel = bootstrap.bind(new InetSocketAddress(port)).sync().channel();
            }
        } catch (Exception t) {
            Exceptions.handle()
                      .to(LOG)
                      .error(t)
                      .withSystemErrorMessage("Cannot setup HTTP (%d): %s (%s)", port)
                      .handle();
        }
    }

    private void createHTTPSChannel() {
        try {
            ServerBootstrap bootstrap = createServerBootstrap(new SSLWebServerInitializer());
            // Bind and start to accept incoming connections.
            if (Strings.isFilled(bindAddress)) {
                sslChannel = bootstrap.bind(new InetSocketAddress(bindAddress, sslPort)).sync().channel();
            } else {
                sslChannel = bootstrap.bind(new InetSocketAddress(sslPort)).sync().channel();
            }
        } catch (Exception t) {
            Exceptions.handle().to(LOG).error(t).withSystemErrorMessage("Cannot setup HTTPS: %s (%s)").handle();
        }
    }

    @Override
    public void stopped() {
        stopChannel(channel, "http");
        stopChannel(sslChannel, "https");
        try (Operation op = new Operation(() -> "eventLoop.shutdownGracefully", Duration.ofSeconds(15))) {
            eventLoop.shutdownGracefully();
        }
        try (Operation op = new Operation(() -> "Response.closeAsyncClient", Duration.ofSeconds(15))) {
            Response.closeAsyncClient();
        }
    }

    private void stopChannel(Channel channel, String name) {
        try (Operation op = new Operation(() -> "stopChannel(" + name + ")", Duration.ofSeconds(15))) {
            if (channel != null) {
                channel.close().sync();
            }
        } catch (InterruptedException e) {
            Exceptions.ignore(e);
            LOG.SEVERE(Strings.apply("Interrupted while waiting for the %s channel to shut down", name));
            Thread.currentThread().interrupt();
        }
    }

    @Override
    public void awaitTermination() {
        try {
            if (!eventLoop.terminationFuture().await(10, TimeUnit.SECONDS)) {
                LOG.SEVERE("Worker Group did not shutdown within 10 seconds!");
            }
        } catch (InterruptedException e) {
            Exceptions.ignore(e);
            LOG.SEVERE("Interrupted while waiting for the Worker Group to shut down");
            Thread.currentThread().interrupt();
        }
    }

    /**
     * Returns the total bytes received so far
     *
     * @return the total bytes received via the http port
     */
    public static long getBytesIn() {
        return bytesIn.get();
    }

    /**
     * Returns the total bytes sent so far
     *
     * @return the total bytes sent via the http port
     */
    public static long getBytesOut() {
        return bytesOut.get();
    }

    /**
     * Returns the total messages (packets) sent so far
     *
     * @return the total messages sent via the http port
     */
    public static long getMessagesIn() {
        return messagesIn.get();
    }

    /**
     * Returns the total messages (packets) received so far
     *
     * @return the total messages received via the http port
     */
    public static long getMessagesOut() {
        return messagesOut.get();
    }

    /**
     * Returns the total number of connections opened so far
     *
     * @return the total number of connections opened on the http port
     */
    public static long getConnections() {
        return connections.get();
    }

    /**
     * Returns the total number of connections blocked so far
     *
     * @return the total number of connections blocked via a firewall rule on the http port
     */
    public static long getBlockedConnections() {
        return blocks.get();
    }

    /**
     * Returns the number of currently open connection
     *
     * @return the number of open connections on the http port
     */
    public static long getNumberOfOpenConnections() {
        return openConnections.size();
    }

    /**
     * Returns the number of currently open websockets
     *
     * @return the number of currently open websockets
     */
    public static long getNumberOfWebsockets() {
        return websockets.get();
    }

    /**
     * Returns the total number of HTTP requests received by the web server
     *
     * @return the total number of requests received
     */
    public static long getRequests() {
        return requests.get();
    }

    /**
     * Returns the total number of HTTP chunks received
     *
     * @return the total number of chunks received
     */
    public static long getChunks() {
        return chunks.get();
    }

    /**
     * Returns the number of keepalives supported
     *
     * @return the number of connections not closed in order to keep them alive.
     */
    public static long getKeepalives() {
        return keepalives.get();
    }

    /**
     * Returns the number of idle connections killed
     *
     * @return the number of connections closed because they were found to be idle.
     */
    public static long getIdleTimeouts() {
        return idleTimeouts.get();
    }

    /**
     * Returns the number of HTTP responses with an 4xx status code.
     *
     * @return the number of HTTP responses with an 4xx status code.
     */
    public static long getClientErrors() {
        return clientErrors.get();
    }

    /**
     * Returns the number of HTTP responses with an 5xx status code.
     *
     * @return the number of HTTP responses with an 5xx status code.
     */
    public static long getServerErrors() {
        return serverErrors.get();
    }

    /**
     * Returns the average response time of the last requests.
     *
     * @return the average response time of the last requests in milliseconds.
     */
    public static double getAvgResponseTime() {
        return responseTime.getAvg();
    }

    /**
     * Returns the average time required to generate a response.
     *
     * @return the average time to first byte of the last requests in milliseconds.
     */
    public static double getAvgTimeToFirstByte() {
        return timeToFirstByte.getAvg();
    }

    /**
     * Returns the average time waiting for a idle worker thead of the web server
     *
     * @return the average time spent waiting for a idle worker thead in milliseconds
     */
    public static double getAvgQueueTime() {
        return queueTime.getAvg();
    }

    @Override
    public void gather(MetricsCollector collector) {
        collector.differentialMetric("http_bytes_in",
                                     "http-bytes-in",
                                     "HTTP Bytes-In",
                                     bytesIn.get() / 1024d / 60,
                                     "KB/s");
        collector.differentialMetric("http_bytes_out",
                                     "http-bytes-out",
                                     "HTTP Bytes-Out",
                                     bytesOut.get() / 1024d / 60,
                                     "KB/s");
        collector.differentialMetric("http_connects", "http-connects", "HTTP Connects", connections.get(), "/min");
        collector.differentialMetric("http_requests", "http-requests", "HTTP Requests", requests.get(), "/min");
        collector.differentialMetric("http-blocks", "http-blocks", "HTTP Blocked Requests", blocks.get(), "/min");
        collector.differentialMetric("http_timeouts",
                                     "http-timeouts",
                                     "HTTP Idle Timeouts",
                                     idleTimeouts.get(),
                                     "/min");
        collector.differentialMetric("http_client_errors",
                                     "http-client-errors",
                                     "HTTP Client Errors (4xx)",
                                     clientErrors.get(),
                                     "/min");
        collector.differentialMetric("http_server_errors",
                                     "http-server-errors",
                                     "HTTP Server Errors (5xx)",
                                     serverErrors.get(),
                                     "/min");
        collector.metric("http_open_connections","http-open-connections", "HTTP Open Connections", openConnections.size(), null);
        collector.metric("http_response_time","http-response-time", "HTTP Avg. Reponse Time", responseTime.getAndClear(), "ms");
        collector.metric("http_response_ttfb","http-response-ttfb", "HTTP Avg. Time To First Byte", timeToFirstByte.getAndClear(), "ms");
        collector.metric("http_response_queue","http-response-queue", "HTTP Avg. Queue Time", queueTime.getAndClear(), "ms");
        collector.metric("http_websockets", "http-websockets", "Open Websockets", websockets.get(), null);
    }

    /**
     * Updates the measured bandwidth of all open http(s) connections.
     */
    @Register
    public static class BandwidthUpdater implements EveryTenSeconds {

        @Override
        public void runTimer() throws Exception {
            openConnections.keySet().forEach(WebServerHandler::updateBandwidth);
        }
    }

    /*
     * Used to notify the web server about an open connection
     */
    protected static void addOpenConnection(WebServerHandler webServerHandler) {
        openConnections.put(webServerHandler, webServerHandler);
    }

    /*
     * Used to notify the web server about an closed connection
     */
    protected static void removeOpenConnection(WebServerHandler webServerHandler) {
        openConnections.remove(webServerHandler);
    }

    /**
     * Returns all currently open connections of the HTTP server.
     *
     * @return a list of all currently open connections
     */
    public static Collection<ActiveHTTPConnection> getOpenConnections() {
        return openConnections.values();
    }

    /**
     * Returns the {@link MicrotimingMode} used by the web server
     *
     * @return the current mode of interaction with the microtiming framework
     * @see sirius.kernel.health.Microtiming
     */
    public static MicrotimingMode getMicrotimingMode() {
        return microtimingMode;
    }

    /**
     * Changes the microtiming mode.
     * <p>
     * Note that the microtiming framework still has to be enabled to generate any output.
     *
     * @param microtimingMode the new microtiming mode to use.
     */
    public static void setMicrotimingMode(MicrotimingMode microtimingMode) {
        WebServer.microtimingMode = microtimingMode == null ? MicrotimingMode.URI : microtimingMode;
    }
}
