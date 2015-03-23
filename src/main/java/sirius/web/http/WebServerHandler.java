/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.http;

import io.netty.channel.ChannelDuplexHandler;
import io.netty.channel.ChannelFutureListener;
import io.netty.channel.ChannelHandlerContext;
import io.netty.handler.codec.http.*;
import io.netty.handler.codec.http.multipart.Attribute;
import io.netty.handler.codec.http.multipart.HttpPostRequestDecoder;
import io.netty.handler.timeout.IdleStateEvent;
import sirius.kernel.async.CallContext;
import sirius.kernel.async.TaskContext;
import sirius.kernel.commons.Strings;
import sirius.kernel.commons.Watch;
import sirius.kernel.health.Average;
import sirius.kernel.health.Exceptions;
import sirius.kernel.nls.NLS;

import java.io.File;
import java.io.IOException;
import java.net.SocketAddress;
import java.nio.channels.ClosedChannelException;
import java.util.List;
import java.util.concurrent.TimeUnit;

/**
 * Handles incoming HTTP requests.
 * <p>
 * Takes care of gluing together chunks, handling file uploads etc. In order to participate in handling HTTP requests,
 * one has to provide a {@link WebDispatcher} rather than modifying this class.
 *
 * @author Andreas Haufler (aha@scireum.de)
 * @since 2013/08
 */
class WebServerHandler extends ChannelDuplexHandler implements ActiveHTTPConnection {

    protected static List<WebDispatcher> sortedDispatchers;
    private int numKeepAlive = 5;
    private HttpRequest currentRequest;
    private WebContext currentContext;
    private CallContext currentCall;
    private volatile long connected;
    private volatile long bytesIn;
    private volatile long bytesOut;
    private volatile long currentBytesIn;
    private volatile long currentBytesOut;
    private volatile long uplink;
    private volatile long downlink;
    private volatile long lastBandwidthUpdate;
    private SocketAddress remoteAddress;
    private boolean preDispatched = false;
    private boolean dispatched = false;
    private Average inboundLatency = new Average();
    private Average processLatency = new Average();
    private Watch latencyWatch;
    private boolean ssl;

    /**
     * Creates a new instance and initializes some statistics.
     */
    WebServerHandler(boolean ssl) {
        this.ssl = ssl;
        this.connected = System.currentTimeMillis();
    }

    /**
     * Periodically called by {@link sirius.web.http.WebServer.BandwidthUpdater#runTimer()} to recompute the current
     * bandwidth.
     */
    protected void updateBandwidth() {
        long now = System.currentTimeMillis();
        long lastUpdate = lastBandwidthUpdate;
        long deltaInSeconds = TimeUnit.SECONDS.convert(now - lastUpdate, TimeUnit.MILLISECONDS);
        if (lastUpdate > 0 && deltaInSeconds > 0) {
            uplink = currentBytesIn / deltaInSeconds;
            downlink = currentBytesOut / deltaInSeconds;
        }
        currentBytesIn = 0;
        currentBytesOut = 0;
        lastBandwidthUpdate = now;
    }

    /*
     * Used when this handler is bound to an incoming connection
     */
    @Override
    public void channelRegistered(ChannelHandlerContext ctx) throws Exception {
        WebServer.addOpenConnection(this);
        this.remoteAddress = ctx.channel().remoteAddress();
        super.channelRegistered(ctx);
    }

    /*
     * Get notified about each exception which occurs while processing channel events
     */
    @Override
    public void exceptionCaught(ChannelHandlerContext ctx, Throwable e) throws Exception {
        if (currentCall != null) {
            CallContext.setCurrent(currentCall);
        }
        if (e instanceof ClosedChannelException) {
            WebServer.LOG.FINE(e);
        } else if (e instanceof IOException) {
            WebServer.LOG.FINE(e);
        } else {
            Exceptions.handle(WebServer.LOG, e);
            try {
                if (ctx.channel().isOpen()) {
                    ctx.channel().close();
                }
            } catch (Throwable t) {
                Exceptions.ignore(t);
            }
        }
        currentRequest = null;
    }

    /*
     * Binds the request to the CallContext
     */
    private WebContext setupContext(ChannelHandlerContext ctx, HttpRequest req) {
        currentCall = CallContext.initialize();
        currentCall.addToMDC("uri", req.getUri());
        WebContext wc = currentCall.get(WebContext.class);
        if (ssl) {
            wc.ssl = true;
        }
        wc.setCtx(ctx);
        wc.setRequest(req);
        currentCall.get(TaskContext.class).setSystem("HTTP").setJob(req.getUri());
        return wc;
    }

    /*
     * Will be notified by the IdleStateHandler if a channel is completely idle for a certain amount of time
     */
    @Override
    public void userEventTriggered(ChannelHandlerContext ctx, Object evt) throws Exception {
        if (evt instanceof IdleStateEvent) {
            if (currentCall != null) {
                CallContext.setCurrent(currentCall);
            } else {
                ctx.channel().close();
                return;
            }
            WebContext wc = currentCall.get(WebContext.class);
            if (wc == null) {
                ctx.channel().close();
                return;
            }
            if (!wc.isLongCall() && !wc.responseCompleted) {
                if (WebServer.LOG.isFINE()) {
                    WebServer.LOG.FINE("IDLE: " + wc.getRequestedURI());
                }
                WebServer.idleTimeouts++;
                if (WebServer.idleTimeouts < 0) {
                    WebServer.idleTimeouts = 0;
                }
                ctx.channel().close();
                return;
            }
        }
    }

    /**
     * Used by responses to determine if keepalive is supported.
     * <p>
     * Internally we used a countdown, to limit the max number of keepalives for a connection. Calling this method
     * decrements the internal counter, therefore this must not be called several times per request.
     *
     * @return <tt>true</tt> if keepalive is still supported, <tt>false</tt> otherwise.
     */
    public boolean shouldKeepAlive() {
        return numKeepAlive-- > 0;
    }

    @Override
    public void channelReadComplete(ChannelHandlerContext ctx) throws Exception {
        ctx.flush();
        super.channelReadComplete(ctx);
    }

    /*
     * Called once a connection is closed. Note that due to keep-alive approaches specified by HTTP 1.1, several
     * independent requests can be handled via one connection
     */
    @Override
    public void channelUnregistered(ChannelHandlerContext ctx) throws Exception {
        cleanup();
        WebServer.removeOpenConnection(this);
        CallContext.detach();
        super.channelUnregistered(ctx);
    }

    /*
     * Notified if a new message is available
     */
    @Override
    public void channelRead(ChannelHandlerContext ctx, Object msg) {
        try {
            if (latencyWatch != null) {
                inboundLatency.addValue(latencyWatch.elapsed(TimeUnit.MILLISECONDS, true));
            } else {
                latencyWatch = Watch.start();
            }
            if (msg instanceof HttpRequest) {
                // Reset stats
                bytesIn = 0;
                bytesOut = 0;
                inboundLatency.getAndClearAverage();
                processLatency.getAndClearAverage();
                handleRequest(ctx, (HttpRequest) msg);
            } else if (msg instanceof LastHttpContent) {
                try {
                    if (currentRequest == null) {
                        WebServer.LOG.FINE("Ignoring CHUNK without request: " + msg);
                        return;
                    }
                    if (currentContext.contentHandler != null) {
                        currentContext.contentHandler.handle(((HttpContent) msg).content(), true);
                    } else {
                        processContent(ctx, (HttpContent) msg);
                    }
                } finally {
                    ((HttpContent) msg).release();
                }
                if (!preDispatched) {
                    dispatch();
                }
            } else if (msg instanceof HttpContent) {
                try {
                    if (currentRequest == null) {
                        WebServer.LOG.FINE("Ignoring CHUNK without request: " + msg);
                        return;
                    }
                    if (!(currentRequest.getMethod() == HttpMethod.POST) && !(currentRequest.getMethod() == HttpMethod.PUT)) {
                        currentContext.respondWith()
                                      .error(HttpResponseStatus.BAD_REQUEST, "Only POST or PUT may sent chunked data");
                        currentRequest = null;
                        return;
                    }
                    WebServer.chunks++;
                    if (WebServer.chunks < 0) {
                        WebServer.chunks = 0;
                    }
                    if (currentContext.contentHandler != null) {
                        currentContext.contentHandler.handle(((HttpContent) msg).content(), false);
                    } else {
                        processContent(ctx, (HttpContent) msg);
                    }
                } finally {
                    ((HttpContent) msg).release();
                }
            }
        } catch (Throwable t) {
            if (currentRequest != null && currentContext != null) {
                try {
                    if (!currentContext.responseCompleted) {
                        currentContext.respondWith()
                                      .error(HttpResponseStatus.BAD_REQUEST,
                                             Exceptions.handle(WebServer.LOG, t).getMessage());
                    }
                } catch (Exception e) {
                    Exceptions.ignore(e);
                }
                currentRequest = null;
            }
            ctx.channel().close();
        }
        processLatency.addValue(latencyWatch.elapsed(TimeUnit.MILLISECONDS, true));
    }

    /*
     * Signals that a bad or incomplete request was received
     */
    private void signalBadRequest(ChannelHandlerContext ctx) {
        WebServer.clientErrors++;
        if (WebServer.clientErrors < 0) {
            WebServer.clientErrors = 0;
        }
        ctx.writeAndFlush(new DefaultFullHttpResponse(HttpVersion.HTTP_1_1, HttpResponseStatus.BAD_REQUEST))
           .addListener(ChannelFutureListener.CLOSE);
        currentRequest = null;
    }

    /*
     * Handles a new request - called once the first chunk of data of a request is available.
     */
    private void handleRequest(ChannelHandlerContext ctx, HttpRequest req) {
        try {
            WebServer.requests++;
            if (WebServer.requests < 0) {
                WebServer.requests = 0;
            }
            cleanup();
            if (WebServer.LOG.isFINE()) {
                WebServer.LOG.FINE("OPEN: " + req.getUri());
            }
            // Handle a bad request.
            if (!req.getDecoderResult().isSuccess()) {
                signalBadRequest(ctx);
                return;
            }
            currentRequest = req;
            preDispatched = false;
            dispatched = false;
            currentContext = setupContext(ctx, req);

            try {
                if (!WebServer.getIPFilter().isEmpty()) {
                    if (!WebServer.getIPFilter().accepts(currentContext.getRemoteIP())) {
                        WebServer.blocks++;
                        if (WebServer.blocks < 0) {
                            WebServer.blocks = 0;
                        }
                        if (WebServer.LOG.isFINE()) {
                            WebServer.LOG.FINE("BLOCK: " + req.getUri());
                        }
                        ctx.channel().close();
                        return;
                    }
                }

                if (HttpHeaders.is100ContinueExpected(req)) {
                    if (WebServer.LOG.isFINE()) {
                        WebServer.LOG.FINE("CONTINUE: " + req.getUri());
                    }
                    send100Continue(ctx);
                }

                if (req.getMethod() == HttpMethod.POST || req.getMethod() == HttpMethod.PUT) {
                    preDispatched = preDispatch();
                    if (preDispatched) {
                        return;
                    }
                    String contentType = req.headers().get(HttpHeaders.Names.CONTENT_TYPE);
                    if (Strings.isFilled(contentType) && (contentType.startsWith("multipart/form-data") || contentType.startsWith(
                            "application/x-www-form-urlencoded"))) {
                        if (WebServer.LOG.isFINE()) {
                            WebServer.LOG.FINE("POST/PUT-FORM: " + req.getUri());
                        }
                        HttpPostRequestDecoder postDecoder = new HttpPostRequestDecoder(WebServer.getHttpDataFactory(),
                                                                                        req);
                        currentContext.setPostDecoder(postDecoder);
                    } else {
                        if (WebServer.LOG.isFINE()) {
                            WebServer.LOG.FINE("POST/PUT-DATA: " + req.getUri());
                        }
                        Attribute body = WebServer.getHttpDataFactory().createAttribute(req, "body");
                        if (req instanceof FullHttpRequest) {
                            body.setContent(((FullHttpRequest) req).content().retain());
                        }
                        currentContext.content = body;
                    }
                } else if (!(currentRequest.getMethod() == HttpMethod.GET) && !(currentRequest.getMethod() == HttpMethod.HEAD) && !(currentRequest
                        .getMethod() == HttpMethod.DELETE)) {
                    currentContext.respondWith()
                                  .error(HttpResponseStatus.BAD_REQUEST,
                                         Strings.apply("Cannot %s as method. Use GET, POST, PUT, HEAD, DELETE",
                                                       req.getMethod().name()));
                    currentRequest = null;
                }
            } catch (Throwable t) {
                currentContext.respondWith()
                              .error(HttpResponseStatus.INTERNAL_SERVER_ERROR, Exceptions.handle(WebServer.LOG, t));
                currentRequest = null;
            }
        } catch (Throwable t) {
            Exceptions.handle(WebServer.LOG, t);
            try {
                ctx.channel().close();
            } catch (Exception ex) {
                Exceptions.ignore(ex);
            }
            cleanup();
            currentRequest = null;
        }
    }

    /*
     * Sends an 100 CONTINUE response to conform to the keepalive protocol
     */
    private void send100Continue(ChannelHandlerContext e) {
        if (WebServer.LOG.isFINE()) {
            WebServer.LOG.FINE("100 - CONTINUE: " + currentContext.getRequestedURI());
        }
        HttpResponse response = new DefaultFullHttpResponse(HttpVersion.HTTP_1_1, HttpResponseStatus.CONTINUE);
        e.channel().write(response);
    }

    /*
     * Releases the last context (request) which was processed by this handler.
     */
    private void cleanup() {
        if (currentContext != null) {
            currentContext.release();
            currentContext = null;
        }
    }

    /*
     * Reads another chunk of data for a previously started request
     */
    private void processContent(ChannelHandlerContext ctx, HttpContent chunk) {
        try {
            if (currentContext.getPostDecoder() != null) {
                if (WebServer.LOG.isFINE()) {
                    WebServer.LOG.FINE("POST-CHUNK: " + currentContext.getRequestedURI() + " - " + chunk.content()
                                                                                                        .readableBytes() + " bytes");
                }
                currentContext.getPostDecoder().offer(chunk);
            } else if (currentContext.content != null) {
                if (WebServer.LOG.isFINE()) {
                    WebServer.LOG.FINE("DATA-CHUNK: " + currentContext.getRequestedURI() + " - " + chunk.content()
                                                                                                        .readableBytes() + " bytes");
                }
                currentContext.content.addContent(chunk.content().retain(), chunk instanceof LastHttpContent);

                if (!currentContext.content.isInMemory()) {
                    File file = currentContext.content.getFile();
                    checkUploadFileLimits(file);
                }
            }
        } catch (Throwable ex) {
            currentContext.respondWith()
                          .error(HttpResponseStatus.INTERNAL_SERVER_ERROR, Exceptions.handle(WebServer.LOG, ex));
            currentRequest = null;
        }
    }

    /*
     * Checks if the can still upload more date
     */
    private void checkUploadFileLimits(File file) {
        if (file.getFreeSpace() < WebServer.getMinUploadFreespace() && WebServer.getMinUploadFreespace() > 0) {
            if (WebServer.LOG.isFINE()) {
                WebServer.LOG.FINE("Not enough space to handle: " + currentContext.getRequestedURI());
            }
            currentContext.respondWith()
                          .error(HttpResponseStatus.INSUFFICIENT_STORAGE,
                                 Exceptions.handle()
                                           .withSystemErrorMessage(
                                                   "The web server is running out of temporary space to store the upload")
                                           .to(WebServer.LOG)
                                           .handle());
            currentRequest = null;
        }
        if (file.length() > WebServer.getMaxUploadSize() && WebServer.getMaxUploadSize() > 0) {
            if (WebServer.LOG.isFINE()) {
                WebServer.LOG.FINE("Body is too large: " + currentContext.getRequestedURI());
            }
            currentContext.respondWith()
                          .error(HttpResponseStatus.INSUFFICIENT_STORAGE,
                                 Exceptions.handle()
                                           .withSystemErrorMessage(
                                                   "The uploaded file exceeds the maximal upload size of %d bytes",
                                                   WebServer.getMaxUploadSize())
                                           .to(WebServer.LOG)
                                           .handle());
            currentRequest = null;
        }
    }

    /*
     * Tries to dispatch a POST or PUT request before it is completely received so that the handler can install
     * a ContentHandler to process all incoming data.
     */
    private boolean preDispatch() throws Exception {
        if (WebServer.LOG.isFINE() && currentContext != null) {
            WebServer.LOG.FINE("DISPATCHING: " + currentContext.getRequestedURI());
        }

        for (WebDispatcher wd : sortedDispatchers) {
            try {
                currentCall.get(TaskContext.class).setSubSystem(wd.getClass().getSimpleName());
                if (wd.preDispatch(currentContext)) {
                    if (WebServer.LOG.isFINE()) {
                        WebServer.LOG.FINE("PRE-DISPATCHED: " + currentContext.getRequestedURI() + " to " + wd);
                    }
                    return true;
                }
            } catch (Exception e) {
                Exceptions.handle(WebServer.LOG, e);
            }
        }

        return false;
    }

    /*
     * Dispatches the completely read request.
     */
    private void dispatch() throws Exception {
        if (WebServer.LOG.isFINE() && currentContext != null) {
            WebServer.LOG.FINE("DISPATCHING: " + currentContext.getRequestedURI());
        }
        currentContext.started = System.currentTimeMillis();
        dispatched = true;
        for (WebDispatcher wd : sortedDispatchers) {
            try {
                currentCall.get(TaskContext.class).setSubSystem(wd.getClass().getSimpleName());
                if (wd.dispatch(currentContext)) {
                    if (WebServer.LOG.isFINE()) {
                        WebServer.LOG.FINE("DISPATCHED: " + currentContext.getRequestedURI() + " to " + wd);
                    }
                    currentRequest = null;
                    return;
                }
            } catch (Exception e) {
                Exceptions.handle(WebServer.LOG, e);
            }
        }
    }

    @Override
    public int getNumKeepAlive() {
        return numKeepAlive;
    }

    @Override
    public String getURL() {
        if (currentContext == null) {
            return "";
        }
        if (currentContext.responseCompleted) {
            return currentContext.getRequestedURI() + " (completed)";
        } else if (currentContext.responseCommitted) {
            return currentContext.getRequestedURI() + " (committed)";
        } else if (preDispatched) {
            return currentContext.getRequestedURI() + " (pre-dispatched)";
        } else if (dispatched) {
            return currentContext.getRequestedURI() + " (dispatched)";
        } else {
            return currentContext.getRequestedURI();
        }
    }

    /*
     * Updates inbound traffic (called via LowLevelHandler)
     */
    protected void inbound(long bytes) {
        bytesIn += bytes;
        currentBytesIn += bytes;
    }

    /*
     * Updates outbound traffic (called via LowLevelHandler)
     */
    protected void outbound(long bytes) {
        bytesOut += bytes;
        currentBytesOut += bytes;
    }

    @Override
    public String getConnectedSince() {
        return TimeUnit.SECONDS.convert(System.currentTimeMillis() - connected, TimeUnit.MILLISECONDS) + "s";
    }

    @Override
    public String getBytesIn() {
        if (bytesIn == 0) {
            return "-";
        }
        return NLS.formatSize(bytesIn);
    }

    @Override
    public String getBytesOut() {
        if (bytesOut == 0) {
            return "-";
        }
        return NLS.formatSize(bytesOut);
    }

    @Override
    public String getUplink() {
        if (uplink == 0) {
            return "-";
        }
        return NLS.formatSize(uplink) + "/s";
    }

    @Override
    public String getDownlink() {
        if (downlink == 0) {
            return "-";
        }
        return NLS.formatSize(downlink) + "/s";
    }

    @Override
    public String getLatency() {
        return Strings.apply("%1.2f ms / %1.2f ms", inboundLatency.getAvg(), processLatency.getAvg());
    }

    @Override
    public String getRemoteAddress() {
        return String.valueOf(remoteAddress);
    }
}
