/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.http;

import com.google.common.base.Charsets;
import com.google.common.collect.Sets;
import com.ning.http.client.AsyncHandler;
import com.ning.http.client.AsyncHttpClient;
import com.ning.http.client.AsyncHttpClientConfig;
import com.ning.http.client.FluentCaseInsensitiveStringsMap;
import com.ning.http.client.HttpResponseBodyPart;
import com.ning.http.client.HttpResponseHeaders;
import io.netty.buffer.ByteBuf;
import io.netty.buffer.Unpooled;
import io.netty.channel.ChannelFuture;
import io.netty.channel.ChannelFutureListener;
import io.netty.channel.ChannelHandlerContext;
import io.netty.channel.DefaultFileRegion;
import io.netty.handler.codec.http.DefaultFullHttpResponse;
import io.netty.handler.codec.http.DefaultHttpContent;
import io.netty.handler.codec.http.DefaultHttpHeaders;
import io.netty.handler.codec.http.DefaultHttpResponse;
import io.netty.handler.codec.http.DefaultLastHttpContent;
import io.netty.handler.codec.http.FullHttpResponse;
import io.netty.handler.codec.http.HttpChunkedInput;
import io.netty.handler.codec.http.HttpHeaders;
import io.netty.handler.codec.http.HttpMethod;
import io.netty.handler.codec.http.HttpResponse;
import io.netty.handler.codec.http.HttpResponseStatus;
import io.netty.handler.codec.http.HttpVersion;
import io.netty.handler.codec.http.LastHttpContent;
import io.netty.handler.codec.http.cookie.Cookie;
import io.netty.handler.codec.http.cookie.ServerCookieEncoder;
import io.netty.handler.ssl.SslHandler;
import io.netty.handler.stream.ChunkedFile;
import io.netty.handler.stream.ChunkedStream;
import io.netty.handler.stream.ChunkedWriteHandler;
import org.rythmengine.Rythm;
import sirius.kernel.Sirius;
import sirius.kernel.async.CallContext;
import sirius.kernel.commons.MultiMap;
import sirius.kernel.commons.Strings;
import sirius.kernel.commons.Tuple;
import sirius.kernel.di.std.Part;
import sirius.kernel.health.Exceptions;
import sirius.kernel.health.HandledException;
import sirius.kernel.health.Microtiming;
import sirius.kernel.nls.NLS;
import sirius.kernel.xml.XMLStructuredOutput;
import sirius.web.services.JSONStructuredOutput;
import sirius.web.templates.Resource;
import sirius.web.templates.Resources;
import sirius.web.templates.rythm.RythmConfig;

import javax.annotation.Nullable;
import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.io.RandomAccessFile;
import java.net.URLConnection;
import java.nio.channels.ClosedChannelException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.TimeZone;
import java.util.concurrent.TimeUnit;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Represents a response which is used to reply to a HTTP request.
 * <p>
 * Responses are created by calling {@link sirius.web.http.WebContext#respondWith()}.
 *
 * @see WebContext
 */
public class Response {
    /**
     * Default cache duration for responses which can be cached
     */
    public static final int HTTP_CACHE = 60 * 60;

    /**
     * Expires value used to indicate that a resource can be infinitely long cached
     */
    public static final int HTTP_CACHE_INFINITE = 60 * 60 * 24 * 356 * 20;

    /**
     * Size of the internally used transfer buffers
     */
    public static final int BUFFER_SIZE = 8192;

    /*
     * Caches the GMT TimeZone (lookup is synchronized)
     */
    private static final TimeZone TIME_ZONE_GMT = TimeZone.getTimeZone("GMT");

    /*
     * Stores the associated request
     */
    private WebContext wc;

    /*
     * Stores the underlying channel
     */
    private ChannelHandlerContext ctx;

    /*
     * Stores the outgoing headers to be sent
     */
    private HttpHeaders headers;

    /*
     * Stores the max expiration of this response. A null value indicates to use the defaults suggested
     * by the content creator.
     */
    private Integer cacheSeconds = null;

    /*
     * Stores if this response should be considered "private" by intermediate caches and proxies
     */
    private boolean isPrivate = false;

    /*
     * Determines if the response should be marked as download
     */
    private boolean download = false;

    /*
     * Contains the name of the downloadable file
     */
    private String name;

    /*
     * Caches the date formatter used to output http date headers
     */
    private SimpleDateFormat dateFormatter;

    /*
     * Determines if the response supports keepalive
     */
    private boolean responseKeepalive = true;

    /*
     * Determines if the response is chunked
     */
    private boolean responseChunked = false;

    @Part
    private static Resources resources;

    /**
     * Creates a new response for the given request.
     *
     * @param wc the context representing the request for which this response is created
     */
    protected Response(WebContext wc) {
        this.wc = wc;
        this.ctx = wc.getCtx();
    }

    /*
     * Creates and initializes a HttpResponse with a complete result at hands.
     * Takes care of the keep alive logic, cookies and other default headers
     */
    private DefaultFullHttpResponse createFullResponse(HttpResponseStatus status, boolean keepalive, ByteBuf buffer) {
        DefaultFullHttpResponse response = new DefaultFullHttpResponse(HttpVersion.HTTP_1_1, status, buffer);
        setupResponse(status, keepalive, response);
        response.headers().set(HttpHeaders.Names.CONTENT_LENGTH, buffer.readableBytes());
        return response;
    }

    /*
     * Creates and initializes a HttpResponse which result will follow as byte buffers
     * Takes care of the keep alive logic, cookies and other default headers
     */
    private DefaultHttpResponse createResponse(HttpResponseStatus status, boolean keepalive) {
        DefaultHttpResponse response = new DefaultHttpResponse(HttpVersion.HTTP_1_1, status);
        if (headers == null || !headers.contains(HttpHeaders.Names.CONTENT_LENGTH)) {
            // We cannot keepalive if the response length is unknown...
            keepalive = false;
        }
        setupResponse(status, keepalive, response);

        return response;
    }

    /*
     * Creates and initializes a HttpResponse which result will follow as chunks. If the requestor does not
     * support HTTP 1.1 we fall back to a "normal" response and disable keepalive (as we need to close the
     * connection to signal the end of the response). Check the responseChunked flag to generate a proper
     * response.
     *
     * Takes care of the keep alive logic, cookies and other default headers
     */
    private DefaultHttpResponse createChunkedResponse(HttpResponseStatus status, boolean keepalive) {
        if (wc.getRequest().getProtocolVersion() == HttpVersion.HTTP_1_0) {
            // HTTP 1.0 does not support chunked results...
            return createResponse(status, keepalive);
        }
        DefaultHttpResponse response = new DefaultHttpResponse(HttpVersion.HTTP_1_1, status);
        response.headers().set(HttpHeaders.Names.TRANSFER_ENCODING, HttpHeaders.Values.CHUNKED);
        responseChunked = true;
        setupResponse(status, keepalive, response);
        return response;
    }

    /*
     * Sets all headers and so on for the response
     */
    private void setupResponse(HttpResponseStatus status, boolean keepalive, DefaultHttpResponse response) {
        if (status.code() >= 500) {
            WebServer.serverErrors++;
            if (WebServer.serverErrors < 0) {
                WebServer.serverErrors = 0;
            }
        } else if (status.code() >= 400) {
            WebServer.clientErrors++;
            if (WebServer.clientErrors < 0) {
                WebServer.clientErrors = 0;
            }
        }

        //Apply headers
        if (headers != null) {
            response.headers().add(headers);
        }

        // Add keepalive header if required
        if (responseKeepalive && keepalive && isKeepalive()) {
            response.headers().set(HttpHeaders.Names.CONNECTION, HttpHeaders.Values.KEEP_ALIVE);
        } else {
            responseKeepalive = false;
        }

        // Add cookies
        Collection<Cookie> cookies = wc.getOutCookies();
        if (cookies != null && !cookies.isEmpty()) {
            response.headers().set(HttpHeaders.Names.SET_COOKIE, ServerCookieEncoder.LAX.encode(cookies));
        }

        // Add Server: nodeName as header
        response.headers()
                .set(HttpHeaders.Names.SERVER, CallContext.getNodeName() + " (scireum SIRIUS - powered by Netty)");

        // Add a P3P-Header. This is used to disable the 3rd-Party auth handling of InternetExplorer
        // which is pretty broken and not used (google and facebook does the same).
        if (WebContext.addP3PHeader) {
            response.headers().set("P3P", "CP=\"This site does not have a p3p policy.\"");
        }
        // Add CORS header...: http://enable-cors.org
        if (WebContext.corsAllowAll && !response.headers().contains(HttpHeaders.Names.ACCESS_CONTROL_ALLOW_ORIGIN)) {
            String requestedOrigin = wc.getHeader(HttpHeaders.Names.ORIGIN);
            if (Strings.isFilled(requestedOrigin)) {
                response.headers().set(HttpHeaders.Names.ACCESS_CONTROL_ALLOW_ORIGIN, requestedOrigin);
            }
        }
    }

    /*
     * Boilerplate for commit(response, true)
     */
    private ChannelFuture commit(HttpResponse response) {
        return commit(response, true);
    }

    /*
     * Commits the response. Once this was called, no other response can be created for this request (WebContext).
     */
    private ChannelFuture commit(HttpResponse response, boolean flush) {
        if (wc.responseCommitted) {
            if (response instanceof FullHttpResponse) {
                ((FullHttpResponse) response).release();
            }
            throw Exceptions.handle()
                            .to(WebServer.LOG)
                            .error(new IllegalStateException())
                            .withSystemErrorMessage("Response for %s was already committed!", wc.getRequestedURI())
                            .handle();
        }
        if (WebServer.LOG.isFINE()) {
            WebServer.LOG.FINE("COMMITTING: " + wc.getRequestedURI());
        }
        wc.responseCommitted = true;
        return flush ? ctx.writeAndFlush(response) : ctx.write(response);
    }

    /**
     * Disables keep-alive protocol (even if it would have been otherwise supported).
     *
     * @return the response itself for fluent method calls
     */
    public Response noKeepalive() {
        responseKeepalive = false;
        return this;
    }

    /*
     * Determines if keepalive is requested by the client and wanted by the server
     */
    private boolean isKeepalive() {
        return HttpHeaders.isKeepAlive(wc.getRequest()) && ((WebServerHandler) ctx.handler()).shouldKeepAlive();
    }

    /*
     * Completes the response and closes the underlying channel if necessary
     */
    private void complete(ChannelFuture future, final boolean supportKeepalive) {
        if (wc.responseCompleted) {
            WebServer.LOG.FINE("Response for %s was already completed!", wc.getRequestedURI());
            return;
        }
        wc.responseCompleted = true;
        if (WebServer.LOG.isFINE()) {
            WebServer.LOG.FINE("COMPLETING: " + wc.getRequestedURI());
        }
        // If we're still confident, that keepalive is supported, and we announced this in the response header,
        // we'll keep the connection open. Otherwise it will be closed by the server
        final boolean keepalive = supportKeepalive && responseKeepalive;
        final CallContext cc = CallContext.getCurrent();
        future.addListener(new ChannelFutureListener() {
            @Override
            public void operationComplete(ChannelFuture future) throws Exception {
                if (wc.completionCallback != null) {
                    try {
                        wc.completionCallback.invoke(cc);
                    } catch (Throwable e) {
                        Exceptions.handle(WebServer.LOG, e);
                    }
                }
                wc.release();
                if (wc.microtimingKey != null && Microtiming.isEnabled()) {
                    cc.getWatch().submitMicroTiming("HTTP", WebServer.microtimingMode.getMicrotimingKey(wc));
                }
                if (!wc.isLongCall() && wc.started > 0) {
                    WebServer.responseTime.addValue(System.currentTimeMillis() - wc.started);
                }
                if (!keepalive) {
                    if (WebServer.LOG.isFINE()) {
                        WebServer.LOG.FINE("CLOSING: " + wc.getRequestedURI());
                    }
                    future.channel().close();
                } else {
                    if (WebServer.LOG.isFINE()) {
                        WebServer.LOG.FINE("KEEP-ALIVE: " + wc.getRequestedURI());
                    }
                    WebServer.keepalives++;
                    if (WebServer.keepalives < 0) {
                        WebServer.keepalives = 0;
                    }
                }
            }
        });
    }

    /*
     * Completes the response once the given future completed while supporting keepalive (response size must be known
     * or response must be chunked).
     */
    private void complete(ChannelFuture future) {
        complete(future, true);
    }

    /*
     * Completes the response once the given future completed without supporting keepalive (which is either unwanted
     * or the response size is not known yet).
     */
    private void completeAndClose(ChannelFuture future) {
        complete(future, false);
    }

    /*
     * Determines if the given modified date is past the If-Modified-Since header of the request. If not the
     * request is auto-completed with a 304 status (NOT_MODIFIED)
     */
    private boolean handleIfModifiedSince(long lastModifiedInMillis) {
        long ifModifiedSinceDateSeconds = wc.getDateHeader(HttpHeaders.Names.IF_MODIFIED_SINCE) / 1000;
        if (ifModifiedSinceDateSeconds > 0 && lastModifiedInMillis > 0) {
            if (ifModifiedSinceDateSeconds >= lastModifiedInMillis / 1000) {
                setDateAndCacheHeaders(lastModifiedInMillis,
                                       cacheSeconds == null ? HTTP_CACHE : cacheSeconds,
                                       isPrivate);
                status(HttpResponseStatus.NOT_MODIFIED);
                return false;
            }
        }

        return true;
    }

    /**
     * Forces the use of a given name. This is also used to derive the mime type.
     *
     * @param name the file name to use
     * @return <tt>this</tt> to fluently create the response
     */
    public Response named(String name) {
        this.name = name;
        return this;
    }

    /**
     * Instructs the browser to treat the response as download with the given file name.
     *
     * @param name the file name to send to the browser. If the given name is <tt>null</tt> nothing happens (We
     *             won't force a download).
     * @return <tt>this</tt> to fluently create the response
     */
    public Response download(@Nullable String name) {
        if (Strings.isFilled(name)) {
            this.name = name;
            this.download = true;
        }
        return this;
    }

    /**
     * Instructs the browser to treat the response as inline-download with the given file name.
     *
     * @param name the file name to send to the browser
     * @return <tt>this</tt> to fluently create the response
     */
    public Response inline(String name) {
        this.name = name;
        this.download = false;
        return this;
    }

    /**
     * Marks this response as not-cachable.
     *
     * @return <tt>this</tt> to fluently create the response
     */
    public Response notCached() {
        this.cacheSeconds = 0;
        return this;
    }

    /**
     * Marks this response as only privately cachable (only the browser may cache it, but not a proxy etc.)
     *
     * @return <tt>this</tt> to fluently create the response
     */
    public Response privateCached() {
        this.isPrivate = true;
        this.cacheSeconds = HTTP_CACHE;
        return this;
    }

    /**
     * Marks this response as cachable for the given amount of time.
     *
     * @param numberOfSeconds the number of seconds the response might be cached
     * @return <tt>this</tt> to fluently create the response
     */
    public Response cachedForSeconds(int numberOfSeconds) {
        this.isPrivate = false;
        this.cacheSeconds = numberOfSeconds;
        return this;
    }

    /**
     * Marks this response as cachable.
     *
     * @return <tt>this</tt> to fluently create the response
     */
    public Response cached() {
        this.isPrivate = false;
        this.cacheSeconds = HTTP_CACHE;
        return this;
    }

    /**
     * Marks this response as infinitely cachable.
     * <p>
     * This suggests that it will never change.
     *
     * @return <tt>this</tt> to fluently create the response
     */
    public Response infinitelyCached() {
        this.isPrivate = false;
        this.cacheSeconds = HTTP_CACHE_INFINITE;
        return this;
    }

    /**
     * Sets the specified header.
     *
     * @param name  name of the header
     * @param value value of the header
     * @return <tt>this</tt> to fluently create the response
     */
    public Response setHeader(String name, Object value) {
        headers().set(name, value);
        return this;
    }

    private HttpHeaders headers() {
        if (headers == null) {
            headers = new DefaultHttpHeaders();
        }

        return headers;
    }

    /**
     * Adds the specified header.
     * <p>
     * In contrast to {@link #setHeader(String, Object)} this method can be called multiple times for the same
     * header and its values will be concatenated as specified in the HTTP protocol.
     *
     * @param name  name of the header
     * @param value value of the header
     * @return <tt>this</tt> to fluently create the response
     */
    public Response addHeader(String name, Object value) {
        headers().add(name, value);
        return this;
    }

    /**
     * Only adds the given header if no header with the given name does exist yet.
     *
     * @param name  name of the header
     * @param value value of the header
     * @return <tt>this</tt> to fluently create the response
     */
    public Response addHeaderIfNotExists(String name, Object value) {
        if (!headers().contains(name)) {
            headers().set(name, value);
        }
        return this;
    }

    /**
     * Adds all given headers
     *
     * @param inputHeaders headers to add
     * @return <tt>this</tt> to fluently create the response
     */
    public Response headers(MultiMap<String, Object> inputHeaders) {
        for (Map.Entry<String, Collection<Object>> e : inputHeaders.getUnderlyingMap().entrySet()) {
            for (Object value : e.getValue()) {
                addHeader(e.getKey(), value);
            }
        }
        return this;
    }

    /**
     * Completes this response by sending the given status code without any content
     *
     * @param status the HTTP status to sent
     */
    public void status(HttpResponseStatus status) {
        HttpResponse response = createFullResponse(status, true, Unpooled.EMPTY_BUFFER);
        complete(commit(response));
    }

    /**
     * Sends a 307 (temporary redirect) or 302 (found) to the given url as result, depending on the given HTTP
     * protocol in the request.
     *
     * @param url the URL to redirect to
     */
    public void redirectTemporarily(String url) {
        if (wc.getRequest().getProtocolVersion() == HttpVersion.HTTP_1_0) {
            // Fallback to HTTP/1.0 code 302 found, which does mostly the same job but has a bad image due to
            // URL hijacking via faulty search engines. The main difference is that 307 will enforce the browser
            // to use the same method for the request to the reported location. Where as 302 doesn't specify which
            // method to use, so a POST might be re-sent as GET to the new location
            HttpResponse response = createFullResponse(HttpResponseStatus.FOUND, true, Unpooled.EMPTY_BUFFER);
            response.headers().set(HttpHeaders.Names.LOCATION, url);
            complete(commit(response));
        } else {
            // Prefer the HTTP/1.1 code 307 as temporary redirect
            HttpResponse response =
                    createFullResponse(HttpResponseStatus.TEMPORARY_REDIRECT, true, Unpooled.EMPTY_BUFFER);
            response.headers().set(HttpHeaders.Names.LOCATION, url);
            complete(commit(response));
        }
    }

    /**
     * Sends a 301 (permanent redirect) to the given url as result.
     *
     * @param url the URL to redirect to
     */
    public void redirectPermanently(String url) {
        HttpResponse response = createFullResponse(HttpResponseStatus.MOVED_PERMANENTLY, true, Unpooled.EMPTY_BUFFER);
        response.headers().set(HttpHeaders.Names.LOCATION, url);
        complete(commit(response));
    }

    private static final Pattern RANGE_HEADER = Pattern.compile("bytes=(\\d+)?\\-(\\d+)?");

    private class SendFile {
        private File file;
        private RandomAccessFile raf;
        private String contentType;
        private long contentStart;
        private long expectedContentLength;
        private Tuple<Long, Long> range;

        void send(File fileToSend) {
            try {
                this.file = fileToSend;
                if (file.isHidden() || !file.exists() || !file.isFile()) {
                    error(HttpResponseStatus.NOT_FOUND);
                    return;
                }
                determineContentType();

                if (!handleIfModifiedSince(file.lastModified())) {
                    return;
                }

                raf = new RandomAccessFile(file, "r");
                setDateAndCacheHeaders(file.lastModified(),
                                       cacheSeconds == null ? HTTP_CACHE : cacheSeconds,
                                       isPrivate);

                try {
                    parseRangesAndUpdateHeaders();
                } catch (IllegalArgumentException e) {
                    error(HttpResponseStatus.REQUESTED_RANGE_NOT_SATISFIABLE, e.getMessage());
                    return;
                }

                if (name != null) {
                    setContentDisposition(name, download);
                }
                sendFileResponse();
            } catch (Throwable e) {
                internalServerError(e);
            }
        }

        private void parseRangesAndUpdateHeaders() throws IOException {
            addHeaderIfNotExists(HttpHeaders.Names.ACCEPT_RANGES, HttpHeaders.Values.BYTES);
            contentStart = 0;
            expectedContentLength = raf.length();
            range = parseRange(raf.length());
            if (range == null) {
                addHeaderIfNotExists(HttpHeaders.Names.CONTENT_LENGTH, expectedContentLength);
            } else {
                contentStart = range.getFirst();
                expectedContentLength = range.getSecond() - range.getFirst() + 1;
                setHeader(HttpHeaders.Names.CONTENT_LENGTH, expectedContentLength);
                setHeader(HttpHeaders.Names.CONTENT_RANGE,
                          "bytes " + range.getFirst() + "-" + range.getSecond() + "/" + raf.length());
            }
        }

        private void determineContentType() {
            contentType = MimeHelper.guessMimeType(name != null ? name : file.getName());
            addHeaderIfNotExists(HttpHeaders.Names.CONTENT_TYPE, contentType);
        }

        /*
         * Determines if we're running on SSL
         */
        private boolean isSSL() {
            return ctx.channel().pipeline().get(SslHandler.class) != null;
        }

        private boolean sendFileResponse() throws IOException {
            HttpResponseStatus responseStatus =
                    range != null ? HttpResponseStatus.PARTIAL_CONTENT : HttpResponseStatus.OK;
            HttpResponse response;
            if (canBeCompressed(contentType)) {
                response = createChunkedResponse(responseStatus, true);
            } else {
                response = createResponse(responseStatus, true);
            }
            commit(response, false);
            installChunkedWriteHandler();
            ChannelFuture writeFuture = executeChunkedWrite();
            writeFuture.addListener(channelFuture -> raf.close());
            complete(writeFuture);
            return false;
        }

        private ChannelFuture executeChunkedWrite() throws IOException {
            if (responseChunked) {
                // Send chunks of data which can be compressed
                ctx.write(new HttpChunkedInput(new ChunkedFile(raf, contentStart, expectedContentLength, BUFFER_SIZE)));
            } else if (isSSL()) {
                ctx.write(new ChunkedFile(raf, contentStart, expectedContentLength, BUFFER_SIZE));
            } else {
                // Send file using zero copy approach!
                ctx.write(new DefaultFileRegion(raf.getChannel(), contentStart, expectedContentLength));
            }
            return ctx.writeAndFlush(LastHttpContent.EMPTY_LAST_CONTENT);
        }

        private void installChunkedWriteHandler() {
            if (ctx.channel().pipeline().get(ChunkedWriteHandler.class) == null) {
                ctx.channel().pipeline().addBefore("handler", "chunkedWriter", new ChunkedWriteHandler());
            }
        }

        private Tuple<Long, Long> parseRange(long availableLength) {
            String header = wc.getHeader(HttpHeaders.Names.RANGE);
            if (Strings.isEmpty(header)) {
                return null;
            }
            Matcher m = RANGE_HEADER.matcher(header);
            if (!m.matches()) {
                throw new IllegalArgumentException(Strings.apply("Range does not match the expected format: %s",
                                                                 header));
            }
            Tuple<Long, Long> result = Tuple.create();
            if (Strings.isFilled(m.group(1))) {
                result.setFirst(Long.parseLong(m.group(1)));
            } else {
                result.setFirst(availableLength - Long.parseLong(m.group(2)));
                result.setSecond(availableLength - 1);
                return result;
            }
            result.setFirst(Long.parseLong(m.group(1)));
            if (Strings.isFilled(m.group(2))) {
                result.setSecond(Long.parseLong(m.group(2)));
            } else {
                result.setSecond(availableLength - 1);
            }
            if (result.getSecond() < result.getFirst()) {
                return null;
            }
            if (result.getSecond() >= availableLength) {
                throw new IllegalArgumentException(Strings.apply("End of range is beyond the end of available data: %s",
                                                                 header));
            }

            return result;
        }

        /*
         * Determines if the current request should be compressed or not
         */
        private boolean canBeCompressed(String contentType) {
            String acceptEncoding = wc.getRequest().headers().get(HttpHeaders.Names.ACCEPT_ENCODING);
            if (acceptEncoding == null || (!acceptEncoding.contains(HttpHeaders.Values.GZIP)
                                           && !acceptEncoding.contains(HttpHeaders.Values.DEFLATE))) {
                return false;
            }
            return MimeHelper.isCompressable(contentType);
        }
    }

    /**
     * Sends the given file as response.
     * <p>
     * Based on the file, full HTTP caching is supported, taking care of If-Modified-Since headers etc.
     * <p>
     * If the request does not use HTTPS, the server tries to support a zero-copy approach leading to maximal
     * throughput as no copying between user space and kernel space buffers is required.
     *
     * @param file the file to send
     */
    public void file(File file) {
        new SendFile().send(file);
    }

    /*
     * Signals an internal server error if one of the response method fails.
     */
    private void internalServerError(Throwable t) {
        WebServer.LOG.FINE(t);
        if (!(t instanceof ClosedChannelException)) {
            if (t instanceof HandledException) {
                error(HttpResponseStatus.INTERNAL_SERVER_ERROR, (HandledException) t);
            } else {
                String requestUri = "?";
                if (wc != null && wc.getRequest() != null) {
                    requestUri = wc.getRequest().getUri();
                }
                Exceptions.handle()
                          .to(WebServer.LOG)
                          .withSystemErrorMessage("An excption occurred while responding to: %s - %s (%s)", requestUri)
                          .handle();
                error(HttpResponseStatus.INTERNAL_SERVER_ERROR, Exceptions.handle(WebServer.LOG, t));
            }
        }
        if (!ctx.channel().isOpen()) {
            ctx.channel().close();
        }
    }

    /*
     * Sets the Date and Cache headers for the HTTP Response
     */
    private void setDateAndCacheHeaders(long lastModifiedMillis, int cacheSeconds, boolean isPrivate) {
        if (headers().contains(HttpHeaders.Names.EXPIRES) || headers().contains(HttpHeaders.Names.CACHE_CONTROL)) {
            return;
        }
        SimpleDateFormat dateFormatter = getHTTPDateFormat();

        if (cacheSeconds > 0) {
            // Date header
            Calendar time = new GregorianCalendar();
            addHeaderIfNotExists(HttpHeaders.Names.DATE, dateFormatter.format(time.getTime()));

            // Add cached headers
            time.add(Calendar.SECOND, cacheSeconds);
            addHeaderIfNotExists(HttpHeaders.Names.EXPIRES, dateFormatter.format(time.getTime()));
            if (isPrivate) {
                addHeaderIfNotExists(HttpHeaders.Names.CACHE_CONTROL, "private, max-age=" + cacheSeconds);
            } else {
                addHeaderIfNotExists(HttpHeaders.Names.CACHE_CONTROL, "public, max-age=" + cacheSeconds);
            }
        } else {
            addHeaderIfNotExists(HttpHeaders.Names.CACHE_CONTROL, HttpHeaders.Values.NO_CACHE + ", max-age=0");
        }
        if (lastModifiedMillis > 0 && !headers().contains(HttpHeaders.Names.LAST_MODIFIED)) {
            addHeaderIfNotExists(HttpHeaders.Names.
                                         LAST_MODIFIED, dateFormatter.format(new Date(lastModifiedMillis)));
        }
    }

    /*
     * Creates a DateFormat to parse HTTP dates.
     */
    private SimpleDateFormat getHTTPDateFormat() {
        if (dateFormatter == null) {
            dateFormatter = new SimpleDateFormat(WebContext.HTTP_DATE_FORMAT, Locale.US);
            dateFormatter.setTimeZone(TIME_ZONE_GMT);
        }
        return dateFormatter;
    }

    /*
     * Sets the content disposition header for the HTTP Response
     */
    private void setContentDisposition(String name, boolean download) {
        String cleanName = name.replaceAll("[^A-Za-z0-9\\-_\\.]", "_");
        String utf8Name = Strings.urlEncode(name.replace(" ", "_"));
        addHeaderIfNotExists("Content-Disposition",
                             (download ? "attachment;" : "inline;")
                             + "filename=\""
                             + cleanName
                             + "\";filename*=UTF-8''"
                             + utf8Name);
    }

    /*
     * Sets the content type header for the HTTP Response
     */
    private void setContentTypeHeader(String name) {
        addHeaderIfNotExists(HttpHeaders.Names.CONTENT_TYPE, MimeHelper.guessMimeType(name));
    }

    /**
     * Tries to resolve the given name into a {@link sirius.web.templates.Resource} using
     * the {@link sirius.web.templates.Resources} lookup framework.
     * <p>
     * Sends the resource found or a 404 NOT_FOUND otherwise.
     *
     * @param name the path of the resource to lookup
     */
    public void sendContent(String name) {
        Optional<Resource> res = resources.resolve(name);
        if (res.isPresent()) {
            try {
                if ("file".equals(res.get().getUrl().getProtocol())) {
                    file(new File(res.get().getUrl().toURI()));
                } else {
                    resource(res.get().getUrl().openConnection());
                }
            } catch (Throwable e) {
                Exceptions.handle()
                          .to(WebServer.LOG)
                          .withSystemErrorMessage(
                                  "An excption occurred while sending content! Content-Name: %s, URL: %s - %s (%s)",
                                  name,
                                  wc == null || wc.getRequest() == null ? "?" : wc.getRequest().getUri())
                          .handle();
                error(HttpResponseStatus.INTERNAL_SERVER_ERROR, Exceptions.handle(WebServer.LOG, e));
            }
        } else {
            error(HttpResponseStatus.NOT_FOUND);
        }
    }

    /**
     * Sends the given resource (potentially from classpath) as result.
     * <p>
     * This will support HTTP caching if enabled (default).
     *
     * @param urlConnection the connection to get the data from.
     */
    public void resource(URLConnection urlConnection) {
        try {
            long fileLength = urlConnection.getContentLength();
            addHeaderIfNotExists(HttpHeaders.Names.CONTENT_LENGTH, fileLength);
            setContentTypeHeader(name != null ? name : urlConnection.getURL().getFile());
            setDateAndCacheHeaders(urlConnection.getLastModified(),
                                   cacheSeconds == null ? HTTP_CACHE : cacheSeconds,
                                   isPrivate);
            if (name != null) {
                setContentDisposition(name, download);
            }

            DefaultHttpResponse response = createResponse(HttpResponseStatus.OK, true);

            // Write the initial line and the header.
            commit(response);
            // Write the content.
            ctx.write(new HttpChunkedInput(new ChunkedStream(urlConnection.getInputStream(), BUFFER_SIZE)));
            // Write last chunk to signal the end of content
            ChannelFuture writeFuture = ctx.writeAndFlush(LastHttpContent.EMPTY_LAST_CONTENT);
            complete(writeFuture);
        } catch (Throwable t) {
            Exceptions.handle()
                      .to(WebServer.LOG)
                      .withSystemErrorMessage(
                              "An excption occurred while sending a resource! Resource: %s, URL: %s - %s (%s)",
                              urlConnection == null ? "null" : urlConnection.getURL().toString(),
                              wc == null || wc.getRequest() == null ? "?" : wc.getRequest().getUri())
                      .handle();
            error(HttpResponseStatus.INTERNAL_SERVER_ERROR, Exceptions.handle(WebServer.LOG, t));
        }
    }

    /**
     * Sends an 401 UNAUTHORIZED response with a WWW-Authenticate header for the given realm.
     * <p>
     * This will generally force the client to perform a HTTP Basic authentication.
     *
     * @param realm the realm to report to the client. This will be used to select an appropriate username
     *              and password
     */
    public void unauthorized(String realm) {
        addHeader("WWW-Authenticate", "Basic realm=\"" + realm + "\"");
        error(HttpResponseStatus.UNAUTHORIZED, "Please specify username and password");
    }

    /**
     * Sends the given HTTP status as error.
     * <p>
     * If possible a specific template /view/errors/ERRORCODE.html. If not available, /view/errors/default.html
     * will be rendered.
     *
     * @param status the HTTP status to send.
     */
    public void error(HttpResponseStatus status) {
        error(status, "");
    }

    /**
     * Sends the given HTTP status as error. Uses the given exception to provide further insight what went wrong.
     * <p>
     * If possible a specific template /view/errors/ERRORCODE.html. If not available, /view/errors/default.html
     * will be rendered.
     *
     * @param status the HTTP status to send
     * @param t      the exception to display. Use {@link sirius.kernel.health.Exceptions} to create a
     *               handled exception.
     */
    public void error(HttpResponseStatus status, HandledException t) {
        error(status, NLS.toUserString(t));
    }

    /**
     * Sends the given HTTP status as error. Uses the given message to provide further insight what went wrong.
     * <p>
     * If possible a specific template /view/errors/ERRORCODE.html. If not available, /view/errors/default.html
     * will be rendered.
     *
     * @param status  the HTTP status to send
     * @param message A message or description of what went wrong
     */
    public void error(HttpResponseStatus status, String message) {
        try {
            if (wc.responseCommitted) {
                if (ctx.channel().isOpen()) {
                    ctx.channel().close();
                }
                return;
            }
            if (!ctx.channel().isWritable()) {
                return;
            }
            if (wc.getRequest().getMethod() == HttpMethod.HEAD) {
                status(status);
                return;
            }
            String content = Rythm.renderIfTemplateExists("view/errors/" + status.code() + ".html", status, message);
            if (Strings.isEmpty(content)) {
                content = Rythm.renderIfTemplateExists("view/errors/error.html", status, message);
            }
            if (Strings.isEmpty(content)) {
                content = Rythm.renderIfTemplateExists("view/errors/default.html", status, message);
            }
            setHeader(HttpHeaders.Names.CONTENT_TYPE, "text/html; charset=UTF-8");
            ByteBuf channelBuffer = wrapUTF8String(content);
            HttpResponse response = createFullResponse(status, false, channelBuffer);
            completeAndClose(commit(response));
        } catch (Throwable e) {
            Exceptions.handle()
                      .to(WebServer.LOG)
                      .withSystemErrorMessage(
                              "An excption occurred while sending an HTTP error! Original Status Code: %s, Original Error: %s, URL: %s - %s (%s)",
                              status == null ? "null" : status.code(),
                              message,
                              wc == null || wc.getRequest() == null ? "?" : wc.getRequest().getUri())
                      .handle();
            if (wc.responseCommitted) {
                if (ctx.channel().isOpen()) {
                    ctx.channel().close();
                }
                return;
            }
            if (!ctx.channel().isWritable()) {
                return;
            }
            ByteBuf channelBuffer = wrapUTF8String(Exceptions.handle(WebServer.LOG, e).getMessage());
            HttpResponse response = createFullResponse(HttpResponseStatus.INTERNAL_SERVER_ERROR, false, channelBuffer);
            response.headers().set(HttpHeaders.Names.CONTENT_TYPE, "text/plain; charset=UTF-8");
            HttpHeaders.setContentLength(response, channelBuffer.readableBytes());
            completeAndClose(commit(response));
        }
    }

    /*
     * Converts a string into a ByteBuf
     */
    private ByteBuf wrapUTF8String(String content) {
        // Returns a heap buffer - but strings a almost always compressed (HTML templtes etc.) so this
        // is probably faster
        return Unpooled.copiedBuffer(content.toCharArray(), Charsets.UTF_8);
    }

    /**
     * Directly sends the given string as response, without any content type. Enable the caller to close the
     * underlying channel (without caring about keep-alive). This can be used to report errors as JSON result to
     * AJAX callers.
     * <p>
     * This should only be used when really required (meaning when you really know what you're doing.
     * The encoding used will be UTF-8).
     *
     * @param status  the HTTP status to send
     * @param content the string contents to send.
     */
    public void direct(HttpResponseStatus status, String content) {
        try {
            setDateAndCacheHeaders(System.currentTimeMillis(),
                                   cacheSeconds == null || Sirius.isDev() ? 0 : cacheSeconds,
                                   isPrivate);
            ByteBuf channelBuffer = wrapUTF8String(content);
            HttpResponse response = createFullResponse(status, true, channelBuffer);
            complete(commit(response));
        } catch (Throwable e) {
            internalServerError(e);
        }
    }

    /**
     * Renders the given Rythm template and sends the output as response.
     * <p>
     * By default caching will be disabled. If the file ends with .html, <tt>text/html; charset=UTF-8</tt> will be set
     * as content type. Otherwise the content type will be guessed from the filename.
     *
     * @param name   the name of the template to render. It's recommended to use files in /view/... and to place them
     *               in the resources directory.
     * @param params contains the parameters sent to the template
     */
    public void template(String name, Object... params) {
        String content = null;
        wc.enableTiming(null);
        try {
            if (params.length == 1 && params[0] instanceof Object[]) {
                params = (Object[]) params[0];
            }
            content = Rythm.render(name, params);
        } catch (Throwable e) {
            throw Exceptions.handle()
                            .to(RythmConfig.LOG)
                            .error(e)
                            .withSystemErrorMessage("Failed to render the template '%s': %s (%s)", name)
                            .handle();
        }
        sendTemplateContent(name, content);
    }

    protected void sendTemplateContent(String name, String content) {
        try {
            if (name.endsWith("html")) {
                setHeader(HttpHeaders.Names.CONTENT_TYPE, "text/html; charset=UTF-8");
            } else {
                setContentTypeHeader(name);
            }
            setDateAndCacheHeaders(System.currentTimeMillis(),
                                   cacheSeconds == null || Sirius.isDev() ? 0 : cacheSeconds,
                                   isPrivate);
            ByteBuf channelBuffer = wrapUTF8String(content);
            HttpResponse response = createFullResponse(HttpResponseStatus.OK, true, channelBuffer);
            complete(commit(response));
        } catch (Throwable e) {
            internalServerError(e);
        }
    }

    /**
     * Tries to find an appropriate Rythm template for the current language and sends the output as response.
     * <p>
     * Based on the given name, <tt>name_LANG.html</tt> or as fallback <tt>name.html</tt> will be loaded. As
     * language, the two-letter language code of {@link sirius.kernel.async.CallContext#getLang()} will be used.
     * <p>
     * By default caching will be disabled. If the file ends with .html, <tt>text/html; charset=UTF-8</tt> will be set
     * as content type. Otherwise the content type will be guessed from the filename.
     *
     * @param name   the name of the template to render. It's recommended to use files in /view/... and to place them
     *               in the resources directory.
     * @param params contains the parameters sent to the template
     */
    public void nlsTemplate(String name, Object... params) {
        String content = null;
        wc.enableTiming(null);
        try {
            if (params.length == 1 && params[0] instanceof Object[]) {
                params = (Object[]) params[0];
            }
            content = Rythm.renderIfTemplateExists(name + "_" + NLS.getCurrentLang() + ".html", params);
            if (Strings.isEmpty(content)) {
                content = Rythm.renderIfTemplateExists(name + "_" + NLS.getDefaultLanguage() + ".html", params);
            }
            if (Strings.isEmpty(content)) {
                content = Rythm.render(name + ".html", params);
            }
        } catch (Throwable e) {
            throw Exceptions.handle()
                            .to(RythmConfig.LOG)
                            .error(e)
                            .withSystemErrorMessage("Failed to render the template '%s': %s (%s)", name)
                            .handle();
        }
        sendTemplateContent(name + ".html", content);
    }

    protected static AsyncHttpClient asyncClient;

    /*
     * Generates and returns a pooling fully asynchronous HTTP client
     */
    protected static AsyncHttpClient getAsyncClient() {
        if (asyncClient == null) {
            asyncClient = new AsyncHttpClient(new AsyncHttpClientConfig.Builder().setAllowPoolingConnections(true)
                                                                                 .setAllowPoolingSslConnections(true)
                                                                                 .setRequestTimeout(-1)
                                                                                 .build());
        }
        return asyncClient;
    }

    /*
     * Closes the async client used to tunnel data (if one was created).
     */
    protected static void closeAsyncClient() {
        if (asyncClient != null) {
            asyncClient.close();
        }
    }

    private static final Set<String> NON_TUNNELLED_HEADERS = Sets.newHashSet(HttpHeaders.Names.TRANSFER_ENCODING,
                                                                             HttpHeaders.Names.SERVER,
                                                                             HttpHeaders.Names.CONTENT_ENCODING,
                                                                             HttpHeaders.Names.EXPIRES,
                                                                             HttpHeaders.Names.CACHE_CONTROL);

    /**
     * Tunnels the contents retrieved from the given URL as result of this response.
     * <p>
     * Caching and range headers will be forwarded and adhered.
     * <p>
     * Uses non-blocking APIs in order to maximize throughput. Therefore this can be called in an unforked
     * dispatcher.
     *
     * @param url the url to tunnel through.
     */
    public void tunnel(final String url) {
        try {
            AsyncHttpClient.BoundRequestBuilder brb = getAsyncClient().prepareGet(url);
            // Support caching...
            long ifModifiedSince = wc.getDateHeader(HttpHeaders.Names.IF_MODIFIED_SINCE);
            if (ifModifiedSince > 0) {
                brb.addHeader(HttpHeaders.Names.IF_MODIFIED_SINCE, getHTTPDateFormat().format(ifModifiedSince));
            }

            // Support range requests...
            String range = wc.getHeader(HttpHeaders.Names.RANGE);
            if (Strings.isFilled(range)) {
                brb.addHeader(HttpHeaders.Names.RANGE, range);
            }
            if (WebServer.LOG.isFINE()) {
                WebServer.LOG.FINE("Tunnel START: %s", url);
            }
            // Tunnel it through...
            brb.execute(new TunnelHandler(url));
        } catch (Throwable t) {
            if (!(t instanceof ClosedChannelException)) {
                // We do not use the message generated by this, to respond to the client, as we do not want to reveal the backing URL
                Exceptions.handle()
                          .to(WebServer.LOG)
                          .withSystemErrorMessage(
                                  "An excption occurred while tunneling a request! Target-URL: %s, URL: %s - %s (%s)",
                                  url,
                                  wc == null || wc.getRequest() == null ? "?" : wc.getRequest().getUri())
                          .handle();
                error(HttpResponseStatus.INTERNAL_SERVER_ERROR, Exceptions.handle(WebServer.LOG, t));
            }
        }
    }

    private class TunnelHandler implements AsyncHandler<String> {

        private final String url;
        private final CallContext cc;
        private int responseCode = HttpResponseStatus.OK.code();
        private boolean contentLengthKnown;

        private TunnelHandler(String url) {
            this.url = url;
            this.cc = CallContext.getCurrent();
        }

        @Override
        public STATE onHeadersReceived(HttpResponseHeaders h) throws Exception {
            CallContext.setCurrent(cc);

            if (wc.responseCommitted) {
                if (WebServer.LOG.isFINE()) {
                    WebServer.LOG.FINE("Tunnel - BLOCKED HEADERS (already sent) for %s", wc.getRequestedURI());
                }
                return STATE.CONTINUE;
            }
            if (WebServer.LOG.isFINE()) {
                WebServer.LOG.FINE("Tunnel - HEADERS for %s", wc.getRequestedURI());
            }
            FluentCaseInsensitiveStringsMap headers = h.getHeaders();

            long lastModified = 0;

            for (Map.Entry<String, List<String>> entry : headers.entrySet()) {
                if ((Sirius.isDev() || !entry.getKey().startsWith("x-"))
                    && !NON_TUNNELLED_HEADERS.contains(entry.getKey())) {
                    for (String value : entry.getValue()) {
                        if (HttpHeaders.Names.LAST_MODIFIED.equals(entry.getKey())) {
                            try {
                                lastModified = getHTTPDateFormat().parse(value).getTime();
                            } catch (Throwable e) {
                                Exceptions.ignore(e);
                            }
                        } else {
                            addHeaderIfNotExists(entry.getKey(), value);
                        }
                    }
                    if (HttpHeaders.Names.CONTENT_LENGTH.equals(entry.getKey())) {
                        contentLengthKnown = true;
                    }
                }
            }

            if (!handleIfModifiedSince(lastModified)) {
                return STATE.ABORT;
            }

            if (!headers().contains(HttpHeaders.Names.CONTENT_TYPE)) {
                setContentTypeHeader(name != null ? name : url);
            }
            setDateAndCacheHeaders(lastModified, cacheSeconds == null ? HTTP_CACHE : cacheSeconds, isPrivate);

            if (name != null) {
                setContentDisposition(name, download);
            }

            return STATE.CONTINUE;
        }

        @Override
        public STATE onBodyPartReceived(HttpResponseBodyPart bodyPart) throws Exception {
            try {
                CallContext.setCurrent(cc);

                if (WebServer.LOG.isFINE()) {
                    WebServer.LOG.FINE("Tunnel - CHUNK: %s for %s (Last: %s)",
                                       bodyPart,
                                       wc.getRequestedURI(),
                                       bodyPart.isLast());
                }
                if (!ctx.channel().isOpen()) {
                    return STATE.ABORT;
                }

                ByteBuf data = Unpooled.wrappedBuffer(bodyPart.getBodyByteBuffer());

                if (!wc.responseCommitted) {
                    //Send a response first
                    if (bodyPart.isLast()) {
                        HttpResponse response =
                                createFullResponse(HttpResponseStatus.valueOf(responseCode), true, data);
                        HttpHeaders.setContentLength(response, bodyPart.getBodyByteBuffer().remaining());
                        complete(commit(response));
                        return STATE.CONTINUE;
                    } else {
                        if (contentLengthKnown) {
                            commit(createResponse(HttpResponseStatus.valueOf(responseCode), true));
                        } else {
                            commit(createChunkedResponse(HttpResponseStatus.valueOf(responseCode), true));
                        }
                    }
                }

                if (bodyPart.isLast()) {
                    if (responseChunked) {
                        ChannelFuture writeFuture = ctx.writeAndFlush(new DefaultLastHttpContent(data));
                        complete(writeFuture);
                    } else {
                        ctx.channel().write(data);
                        ChannelFuture writeFuture = ctx.writeAndFlush(LastHttpContent.EMPTY_LAST_CONTENT);
                        complete(writeFuture);
                    }
                } else {
                    Object msg = responseChunked ? new DefaultHttpContent(data) : data;
                    contentionAwareWrite(msg);
                }
                return STATE.CONTINUE;
            } catch (HandledException e) {
                Exceptions.ignore(e);
                return STATE.ABORT;
            } catch (Throwable e) {
                Exceptions.handle(e);
                return STATE.ABORT;
            }
        }

        @Override
        public STATE onStatusReceived(com.ning.http.client.HttpResponseStatus httpResponseStatus) throws Exception {
            CallContext.setCurrent(cc);

            if (WebServer.LOG.isFINE()) {
                WebServer.LOG.FINE("Tunnel - STATUS %s for %s",
                                   httpResponseStatus.getStatusCode(),
                                   wc.getRequestedURI());
            }
            if (httpResponseStatus.getStatusCode() >= 200 && httpResponseStatus.getStatusCode() < 300) {
                responseCode = httpResponseStatus.getStatusCode();
                return STATE.CONTINUE;
            }
            if (httpResponseStatus.getStatusCode() == HttpResponseStatus.NOT_MODIFIED.code()) {
                status(HttpResponseStatus.NOT_MODIFIED);
                return STATE.ABORT;
            }
            error(HttpResponseStatus.valueOf(httpResponseStatus.getStatusCode()));
            return STATE.ABORT;
        }

        @Override
        public String onCompleted() throws Exception {
            CallContext.setCurrent(cc);

            if (WebServer.LOG.isFINE()) {
                WebServer.LOG.FINE("Tunnel - COMPLETE for %s", wc.getRequestedURI());
            }
            if (!wc.responseCommitted) {
                HttpResponse response =
                        createFullResponse(HttpResponseStatus.valueOf(responseCode), true, Unpooled.EMPTY_BUFFER);
                HttpHeaders.setContentLength(response, 0);
                complete(commit(response));
            } else if (!wc.responseCompleted) {
                ChannelFuture writeFuture = ctx.writeAndFlush(LastHttpContent.EMPTY_LAST_CONTENT);
                complete(writeFuture);
            }
            return "";
        }

        @Override
        public void onThrowable(Throwable t) {
            CallContext.setCurrent(cc);

            WebServer.LOG.WARN("Tunnel - ERROR %s for %s",
                               t.getMessage() + " (" + t.getMessage() + ")",
                               wc.getRequestedURI());
            if (!(t instanceof ClosedChannelException)) {
                error(HttpResponseStatus.INTERNAL_SERVER_ERROR, Exceptions.handle(WebServer.LOG, t));
            }
        }
    }

    /**
     * Creates a JSON output which can be used to generate well formed json.
     * <p>
     * By default, caching will be disabled. If the generated JSON is small enough, it will be transmitted in
     * one go. Otherwise a chunked response will be sent.
     *
     * @return a structured output which will be sent as JSON response
     */
    public JSONStructuredOutput json() {
        String callback = wc.get("callback").getString();
        String encoding = wc.get("encoding").first().asString(Charsets.UTF_8.name());
        return new JSONStructuredOutput(outputStream(HttpResponseStatus.OK, "application/json;charset=" + encoding),
                                        callback,
                                        encoding);
    }

    /**
     * Creates a XML output which can be used to generate well formed XML.
     * <p>
     * By default, caching will be disabled. If the generated XML is small enough, it will be transmitted in
     * one go. Otherwise a chunked response will be sent.
     *
     * @return a structured output which will be sent as XML response
     */
    public XMLStructuredOutput xml() {
        return new XMLStructuredOutput(outputStream(HttpResponseStatus.OK, MimeHelper.TEXT_XML));
    }

    /**
     * Creates an OutputStream which is sent to the client.
     * <p>
     * If the contents are small enough, everything will be sent in one response. Otherwise a chunked response
     * will be sent. The size of the underlying buffer will be determined by {@link #BUFFER_SIZE}.
     * <p>
     * <b>WARNING:</b> Do not used this kind of response directly from a {@link WebDispatcher}! You need to fork a
     * new thread using {@link sirius.kernel.async.Tasks} as the internal workings might block in
     * {@code OutputStream.write} until the message is fully written to the channel. This might lead to a deadlock
     * if the kernel buffer needs to be flushed as well (as this needs the worker thread to handle the write which is
     * blocked internally due to waiting for the chunk to be written).
     * <p>
     * By default, caching will be supported.
     *
     * @param status      the HTTP status to send
     * @param contentType the content type to use. If <tt>null</tt>, we rely on a previously set header.
     * @return an output stream which will be sent as response
     */
    public OutputStream outputStream(final HttpResponseStatus status, @Nullable final String contentType) {
        if (wc.responseCommitted) {
            throw Exceptions.handle()
                            .to(WebServer.LOG)
                            .error(new IllegalStateException())
                            .withSystemErrorMessage("Response for %s was already committed!", wc.getRequestedURI())
                            .handle();
        }

        return new ChunkedOutputStream(contentType, status);
    }

    private class ChunkedOutputStream extends OutputStream {
        private final String contentType;
        private final HttpResponseStatus status;
        volatile boolean open;
        volatile long bytesWritten;
        ByteBuf buffer;

        private ChunkedOutputStream(String contentType, HttpResponseStatus status) {
            this.contentType = contentType;
            this.status = status;
            open = true;
            bytesWritten = 0;
            buffer = null;
        }

        private void ensureCapacity(int length) {
            if (buffer != null && buffer.writableBytes() < length) {
                flushBuffer(false);
            }
            if (buffer == null) {
                buffer = ctx.alloc().buffer(BUFFER_SIZE);
            }
        }

        private void flushBuffer(boolean last) {
            if ((buffer == null || buffer.readableBytes() == 0) && !last) {
                if (buffer != null) {
                    buffer.release();
                    buffer = null;
                }
                return;
            }
            if (!ctx.channel().isOpen()) {
                open = false;
                throw Exceptions.createHandled().withSystemErrorMessage("Channel closed by peer").handle();
            }
            if (!wc.responseCommitted) {
                createResponse(last);
                if (last) {
                    return;
                }
            }

            if (last) {
                if (buffer != null) {
                    complete(ctx.writeAndFlush(new DefaultLastHttpContent(buffer)));
                } else {
                    complete(ctx.writeAndFlush(LastHttpContent.EMPTY_LAST_CONTENT));
                }
            } else {
                Object message = new DefaultHttpContent(buffer);
                contentionAwareWrite(message);
            }
            buffer = null;
        }

        private void createResponse(boolean last) {
            if (Strings.isFilled(contentType)) {
                addHeaderIfNotExists(HttpHeaders.Names.CONTENT_TYPE, contentType);
            }
            setDateAndCacheHeaders(System.currentTimeMillis(),
                                   cacheSeconds == null || Sirius.isDev() ? 0 : cacheSeconds,
                                   isPrivate);
            if (name != null) {
                setContentDisposition(name, download);
            }
            if (last) {
                ByteBuf initialBuffer = buffer;
                if (initialBuffer == null) {
                    initialBuffer = Unpooled.EMPTY_BUFFER;
                }
                HttpResponse response = createFullResponse(status, true, initialBuffer);
                HttpHeaders.setContentLength(response, initialBuffer.readableBytes());
                complete(commit(response));
            } else {
                HttpResponse response = createChunkedResponse(HttpResponseStatus.OK, true);
                commit(response, false);
            }
        }

        @Override
        public void flush() throws IOException {
            flushBuffer(false);
        }

        @Override
        public void write(int b) throws IOException {
            if (!open) {
                return;
            }
            bytesWritten++;
            ensureCapacity(1);
            buffer.writeByte(b);
        }

        @Override
        public void write(byte[] b) throws IOException {
            write(b, 0, b.length);
        }

        @Override
        public void write(byte[] b, int off, int len) throws IOException {
            if (!open) {
                return;
            }
            if (len <= 0) {
                return;
            }
            // If the given array is larger than our buffer, we repeatedly call write and limit the length to
            // our buffer size.
            if (len > BUFFER_SIZE) {
                write(b, off, BUFFER_SIZE);
                write(b, off + BUFFER_SIZE, len - BUFFER_SIZE);
                return;
            }
            ensureCapacity(len);
            bytesWritten += len;
            buffer.writeBytes(b, off, len);
        }

        @Override
        public void close() throws IOException {
            if (!open) {
                return;
            }
            open = false;
            super.close();
            if (ctx.channel().isOpen()) {
                flushBuffer(true);
            } else if (buffer != null) {
                buffer.release();
                buffer = null;
            }
        }
    }

    private void contentionAwareWrite(Object message) {
        if (!ctx.channel().isWritable()) {
            ChannelFuture future = ctx.writeAndFlush(message);
            while (!ctx.channel().isWritable() && ctx.channel().isOpen()) {
                try {
                    future.await(5, TimeUnit.SECONDS);
                } catch (InterruptedException e) {
                    ctx.channel().close();
                    Exceptions.ignore(e);
                    throw Exceptions.createHandled()
                                    .withSystemErrorMessage("Interrupted while waiting for a chunk to be written")
                                    .handle();
                }
            }
        } else {
            ctx.write(message);
        }
    }

    @Override
    public String toString() {
        return "Response for: " + wc.toString();
    }
}
