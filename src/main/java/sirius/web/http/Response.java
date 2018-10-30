/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.http;

import com.google.common.base.Charsets;
import com.ning.http.client.AsyncHttpClient;
import com.ning.http.client.AsyncHttpClientConfig;
import io.netty.buffer.ByteBuf;
import io.netty.buffer.Unpooled;
import io.netty.channel.ChannelFuture;
import io.netty.channel.ChannelHandlerContext;
import io.netty.handler.codec.http.DefaultFullHttpResponse;
import io.netty.handler.codec.http.DefaultHttpHeaders;
import io.netty.handler.codec.http.DefaultHttpResponse;
import io.netty.handler.codec.http.FullHttpResponse;
import io.netty.handler.codec.http.HttpChunkedInput;
import io.netty.handler.codec.http.HttpHeaderNames;
import io.netty.handler.codec.http.HttpHeaderValues;
import io.netty.handler.codec.http.HttpHeaders;
import io.netty.handler.codec.http.HttpMethod;
import io.netty.handler.codec.http.HttpResponse;
import io.netty.handler.codec.http.HttpResponseStatus;
import io.netty.handler.codec.http.HttpUtil;
import io.netty.handler.codec.http.HttpVersion;
import io.netty.handler.codec.http.LastHttpContent;
import io.netty.handler.codec.http.cookie.Cookie;
import io.netty.handler.codec.http.cookie.ServerCookieEncoder;
import io.netty.handler.stream.ChunkedStream;
import io.netty.handler.stream.ChunkedWriteHandler;
import sirius.kernel.Sirius;
import sirius.kernel.async.CallContext;
import sirius.kernel.async.ExecutionPoint;
import sirius.kernel.commons.MultiMap;
import sirius.kernel.commons.Strings;
import sirius.kernel.di.std.Part;
import sirius.kernel.health.Exceptions;
import sirius.kernel.health.HandledException;
import sirius.kernel.health.Microtiming;
import sirius.kernel.nls.NLS;
import sirius.kernel.xml.XMLStructuredOutput;
import sirius.tagliatelle.Tagliatelle;
import sirius.tagliatelle.Template;
import sirius.tagliatelle.compiler.CompileException;
import sirius.tagliatelle.rendering.GlobalRenderContext;
import sirius.web.resources.Resource;
import sirius.web.resources.Resources;
import sirius.web.services.JSONStructuredOutput;

import javax.annotation.Nullable;
import java.io.File;
import java.io.OutputStream;
import java.net.URLConnection;
import java.nio.channels.ClosedChannelException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;
import java.util.TimeZone;
import java.util.concurrent.TimeUnit;
import java.util.function.Consumer;
import java.util.stream.Collectors;

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
     * Contains the file extension used by HTML
     */
    private static final String FILETYPE_HTML = ".html";

    /*
     * Contains the content type used for html
     */
    private static final String CONTENT_TYPE_HTML = "text/html; charset=UTF-8";

    /*
     * Stores the associated request
     */
    protected WebContext wc;

    /*
     * Stores the underlying channel
     */
    protected ChannelHandlerContext ctx;

    /*
     * Stores the outgoing headers to be sent
     */
    private HttpHeaders headers;

    /*
     * Stores the max expiration of this response. A null value indicates to use the defaults suggested
     * by the content creator.
     */
    protected Integer cacheSeconds = null;

    /*
     * Stores if this response should be considered "private" by intermediate caches and proxies
     */
    protected boolean isPrivate = false;

    /*
     * Determines if the response should be marked as download
     */
    protected boolean download = false;

    /*
     * Contains the name of the downloadable file
     */
    protected String name;

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
    protected boolean responseChunked = false;

    @Part
    private static Resources resources;

    @Part
    private static UserMessagesCache userMessagesCache;

    @Part
    private static Tagliatelle engine;

    protected static AsyncHttpClient asyncClient;

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
    protected DefaultFullHttpResponse createFullResponse(HttpResponseStatus status, boolean keepalive, ByteBuf buffer) {
        DefaultFullHttpResponse response = new DefaultFullHttpResponse(HttpVersion.HTTP_1_1, status, buffer);
        setupResponse(status, keepalive, response);
        response.headers().set(HttpHeaderNames.CONTENT_LENGTH, buffer.readableBytes());
        return response;
    }

    /*
     * Creates and initializes a HttpResponse which result will follow as byte buffers
     * Takes care of the keep alive logic, cookies and other default headers
     */
    protected DefaultHttpResponse createResponse(HttpResponseStatus status, boolean keepalive) {
        DefaultHttpResponse response = new DefaultHttpResponse(HttpVersion.HTTP_1_1, status);
        if (headers == null || !headers.contains(HttpHeaderNames.CONTENT_LENGTH)) {
            // We cannot keepalive if the response length is unknown...
            setupResponse(status, false, response);
        } else {
            setupResponse(status, keepalive, response);
        }

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
    protected DefaultHttpResponse createChunkedResponse(HttpResponseStatus status, boolean keepalive) {
        if (HttpVersion.HTTP_1_0.equals(wc.getRequest().protocolVersion())) {
            // HTTP 1.0 does not support chunked results...
            return createResponse(status, keepalive);
        }
        DefaultHttpResponse response = new DefaultHttpResponse(HttpVersion.HTTP_1_1, status);
        response.headers().set(HttpHeaderNames.TRANSFER_ENCODING, HttpHeaderValues.CHUNKED);
        responseChunked = true;
        setupResponse(status, keepalive, response);
        return response;
    }

    /*
     * Sets all headers and so on for the response
     */
    private void setupResponse(HttpResponseStatus status, boolean keepalive, DefaultHttpResponse response) {
        updateStatistics(status);

        //Apply headers
        if (headers != null) {
            response.headers().add(headers);
        }

        // Add keepalive header if required
        if (responseKeepalive && keepalive && isKeepalive()) {
            response.headers().set(HttpHeaderNames.CONNECTION, HttpHeaderValues.KEEP_ALIVE);
        } else {
            if (!HttpVersion.HTTP_1_0.equals(wc.getRequest().protocolVersion())) {
                response.headers().set(HttpHeaderNames.CONNECTION, HttpHeaderValues.CLOSE);
            }
            responseKeepalive = false;
        }

        setupCookies(response);
        setupHeaders(response);
        setupCors(response);
    }

    private void setupCors(DefaultHttpResponse response) {
        if (!WebContext.corsAllowAll || response.headers().contains(HttpHeaderNames.ACCESS_CONTROL_ALLOW_ORIGIN)) {
            return;
        }
        
        response.headers().set(HttpHeaderNames.VARY, HttpHeaderNames.ORIGIN);
        String requestedOrigin = wc.getHeader(HttpHeaderNames.ORIGIN);
        if (Strings.isFilled(requestedOrigin)) {
            response.headers().set(HttpHeaderNames.ACCESS_CONTROL_ALLOW_ORIGIN, requestedOrigin);
            if (!response.headers().contains(HttpHeaderNames.ACCESS_CONTROL_ALLOW_CREDENTIALS)) {
                response.headers().set(HttpHeaderNames.ACCESS_CONTROL_ALLOW_CREDENTIALS, "true");
            }
        }
    }

    private void setupCookies(DefaultHttpResponse response) {
        Collection<Cookie> cookies = wc.getOutCookies();
        if (cookies != null && !cookies.isEmpty()) {
            response.headers().set(HttpHeaderNames.SET_COOKIE, ServerCookieEncoder.LAX.encode(cookies));
        }
    }

    private void setupHeaders(DefaultHttpResponse response) {
        // Add Server: nodeName as header
        response.headers()
                .set(HttpHeaderNames.SERVER, CallContext.getNodeName() + " (scireum SIRIUS - powered by Netty)");

        // Add a P3P-Header. This is used to disable the 3rd-Party auth handling of InternetExplorer
        // which is pretty broken and not used (google and facebook does the same).
        if (WebContext.addP3PHeader) {
            response.headers().set("P3P", "CP=\"This site does not have a p3p policy.\"");
        }

        if (Strings.isFilled(WebContext.contentSecurityPolicy)) {
            response.headers().set("Content-Security-Policy", WebContext.contentSecurityPolicy);
        }

        // Adds a Strict Transport Security (HSTS) header...
        if (WebContext.forceHSTS) {
            response.headers()
                    .set("Strict-Transport-Security", "max-age=" + WebContext.hstsMaxAge + "; includeSubDomains");
        }

        // NEVER allow a Set-Cookie header within a cached request...
        if (response.headers().contains(HttpHeaderNames.SET_COOKIE)) {
            if (response.headers().contains(HttpHeaderNames.EXPIRES)) {
                WebServer.LOG.WARN("A response with 'set-cookie' and 'expires' was created for URI: %s%n%s%n%s",
                                   wc.getRequestedURI(),
                                   wc,
                                   ExecutionPoint.snapshot());
                response.headers().remove(HttpHeaderNames.EXPIRES);
            }
            String cacheControl = response.headers().get(HttpHeaderNames.CACHE_CONTROL);
            if (cacheControl != null && !cacheControl.startsWith(HttpHeaderValues.NO_CACHE.toString())) {
                WebServer.LOG.WARN("A response with 'set-cookie' and 'cache-control' was created for URI: %s%n%s%n%s",
                                   wc.getRequestedURI(),
                                   wc,
                                   ExecutionPoint.snapshot());
                response.headers().set(HttpHeaderNames.CACHE_CONTROL, HttpHeaderValues.NO_CACHE + ", max-age=0");
            }
        }
    }

    private void updateStatistics(HttpResponseStatus status) {
        if (status.code() >= 500) {
            if (WebServer.serverErrors.incrementAndGet() < 0) {
                WebServer.serverErrors.set(0);
            }
        } else if (status.code() >= 400) {
            if (WebServer.clientErrors.incrementAndGet() < 0) {
                WebServer.clientErrors.set(0);
            }
        }
    }

    /*
     * Boilerplate for commit(response, true)
     */
    protected ChannelFuture commit(HttpResponse response) {
        return commit(response, true);
    }

    /*
     * Commits the response. Once this was called, no other response can be created for this request (WebContext).
     */
    protected ChannelFuture commit(HttpResponse response, boolean flush) {
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
        wc.committed = System.currentTimeMillis();
        wc.releaseContentHandler();
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
        return HttpUtil.isKeepAlive(wc.getRequest()) && ((WebServerHandler) ctx.handler()).shouldKeepAlive();
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
        future.addListener(ignored -> onCompleteCompleted(cc, keepalive, future));
    }

    private void onCompleteCompleted(CallContext callContext, boolean keepalive, ChannelFuture future) {
        if (wc.completionCallback != null) {
            try {
                wc.completionCallback.invoke(callContext);
            } catch (Exception e) {
                Exceptions.handle(WebServer.LOG, e);
            }
        }
        wc.release();
        updateResponseTimeMetrics(callContext);
        handleKeepalive(keepalive, future);
    }

    private void updateResponseTimeMetrics(CallContext callContext) {
        if (wc.microtimingKey != null && Microtiming.isEnabled()) {
            callContext.getWatch().submitMicroTiming("HTTP", WebServer.microtimingMode.getMicrotimingKey(wc));
        }
        if (wc.isLongCall() || wc.scheduled == 0) {
            // No response time measurement for long running or aborted requests...
            return;
        }
        long queuedMillis = wc.scheduled - wc.started;
        long ttfbMillis = wc.getTTFBMillis();
        long responseTimeMillis = System.currentTimeMillis() - wc.started;

        WebServer.queueTime.addValue(queuedMillis);
        WebServer.timeToFirstByte.addValue(ttfbMillis);
        WebServer.responseTime.addValue(responseTimeMillis);

        if (ttfbMillis > WebServer.getMaxTimeToFirstByte() && WebServer.getMaxTimeToFirstByte() > 0) {
            WebServer.LOG.WARN("Long running request: %s (Response Time: %s, Queue Time: %s, TTFB: %s)"
                               + "%nURL:%s"
                               + "%nParameters:"
                               + "%n%s"
                               + "%nMDC:"
                               + "%n%s%n",
                               wc.getRequestedURI(),
                               NLS.convertDuration(responseTimeMillis, true, true),
                               NLS.convertDuration(queuedMillis, true, true),
                               NLS.convertDuration(ttfbMillis, true, true),
                               wc.getRequestedURL(),
                               wc.getParameterNames()
                                 .stream()
                                 .map(param -> param + ": " + Strings.limit(wc.get(param).asString(), 50))
                                 .collect(Collectors.joining("\n")),
                               callContext);
        }
    }

    private void handleKeepalive(boolean keepalive, ChannelFuture future) {
        if (!keepalive) {
            if (WebServer.LOG.isFINE()) {
                WebServer.LOG.FINE("CLOSING: " + wc.getRequestedURI());
            }
            future.channel().close();
        } else {
            if (WebServer.LOG.isFINE()) {
                WebServer.LOG.FINE("KEEP-ALIVE: " + wc.getRequestedURI());
            }
            if (WebServer.keepalives.incrementAndGet() < 0) {
                WebServer.keepalives.set(0);
            }
        }
    }

    /*
     * Completes the response once the given future completed while supporting keepalive (response size must be known
     * or response must be chunked).
     */
    protected void complete(ChannelFuture future) {
        complete(future, true);
    }

    /*
     * Completes the response once the given future completed without supporting keepalive (which is either unwanted
     * or the response size is not known yet).
     */
    private void completeAndClose(ChannelFuture future) {
        complete(future, false);
    }

    /**
     * Determines if the given modified date is past the If-Modified-Since header of the request. If not the
     * request is auto-completed with a 304 status (NOT_MODIFIED)
     *
     * @param lastModifiedInMillis the modification date of the resource being delivered
     * @return <tt>true</tt> if the request was answered via a 304, <tt>false</tt> otherwise
     */
    public boolean handleIfModifiedSince(long lastModifiedInMillis) {
        long ifModifiedSinceDateSeconds = wc.getDateHeader(HttpHeaderNames.IF_MODIFIED_SINCE) / 1000;
        if (ifModifiedSinceDateSeconds > 0 && lastModifiedInMillis > 0) {
            if (ifModifiedSinceDateSeconds >= lastModifiedInMillis / 1000) {
                setDateAndCacheHeaders(lastModifiedInMillis,
                                       cacheSeconds == null ? HTTP_CACHE : cacheSeconds,
                                       isPrivate);
                status(HttpResponseStatus.NOT_MODIFIED);
                return true;
            }
        }

        return false;
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
    public Response setHeader(CharSequence name, Object value) {
        headers().set(name, value);
        return this;
    }

    protected HttpHeaders headers() {
        if (headers == null) {
            headers = new DefaultHttpHeaders();
        }

        return headers;
    }

    /**
     * Adds the specified header.
     * <p>
     * In contrast to {@link #setHeader(CharSequence, Object)} this method can be called multiple times for the same
     * header and its values will be concatenated as specified in the HTTP protocol.
     *
     * @param name  name of the header
     * @param value value of the header
     * @return <tt>this</tt> to fluently create the response
     */
    public Response addHeader(CharSequence name, Object value) {
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
    public Response addHeaderIfNotExists(CharSequence name, Object value) {
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
     * <p>
     * If contrast to {@link #redirectToGet(String)}, this uses <tt>307</tt> as status code - if possible (HTTP 1.1).
     * This will re-issue the same request method which was made to trigger the original request.
     *
     * @param url the URL to redirect to
     */
    public void redirectTemporarily(String url) {
        if (HttpVersion.HTTP_1_0.equals(wc.getRequest().protocolVersion())) {
            // Fallback to HTTP/1.0 code 302 found, which does mostly the same job but has a bad image due to
            // URL hijacking via faulty search engines. The main difference is that 307 will enforce the browser
            // to use the same method for the request to the reported location. Where as 302 doesn't specify which
            // method to use, so a POST might be re-sent as GET to the new location
            redirectToGet(url);
        } else {
            userMessagesCache.cacheUserMessages(wc);

            // Prefer the HTTP/1.1 code 307 as temporary redirect
            HttpResponse response =
                    createFullResponse(HttpResponseStatus.TEMPORARY_REDIRECT, true, Unpooled.EMPTY_BUFFER);
            response.headers().set(HttpHeaderNames.LOCATION, url);
            complete(commit(response));
        }
    }

    /**
     * Sends 302 (found) to the given url as result.
     * <p>
     * In contrast to {@link #redirectTemporarily(String)}, which uses <tt>307</tt> as HTTP response code, a 302 will
     * cause the browser to always GET as method to access the new URL. A <tt>307</tt> preserves the method and e.g.
     * keeps a POST as being a POST.
     *
     * @param url the URL to redirect to
     */
    public void redirectToGet(String url) {
        userMessagesCache.cacheUserMessages(wc);

        HttpResponse response = createFullResponse(HttpResponseStatus.FOUND, true, Unpooled.EMPTY_BUFFER);
        response.headers().set(HttpHeaderNames.LOCATION, url);
        complete(commit(response));
    }

    /**
     * Sends a 301 (permanent redirect) to the given url as result.
     *
     * @param url the URL to redirect to
     */
    public void redirectPermanently(String url) {
        userMessagesCache.cacheUserMessages(wc);

        HttpResponse response = createFullResponse(HttpResponseStatus.MOVED_PERMANENTLY, true, Unpooled.EMPTY_BUFFER);
        response.headers().set(HttpHeaderNames.LOCATION, url);
        complete(commit(response));
    }

    /*
     * Determines if the current request should be compressed or not
     */
    protected boolean canBeCompressed(String contentType) {
        String acceptEncoding = wc.getRequest().headers().get(HttpHeaderNames.ACCEPT_ENCODING);
        if (acceptEncoding == null || (!acceptEncoding.contains(HttpHeaderValues.GZIP) && !acceptEncoding.contains(
                HttpHeaderValues.DEFLATE))) {
            return false;
        }
        return MimeHelper.isCompressable(contentType);
    }

    protected void installChunkedWriteHandler() {
        if (ctx.channel().pipeline().get(ChunkedWriteHandler.class) == null && ctx.channel().isOpen()) {
            ctx.channel().pipeline().addBefore("handler", "chunkedWriter", new ChunkedWriteHandler());
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
        new SendFile(this).send(file);
    }

    /*
     * Signals an internal server error if one of the response method fails.
     */
    protected void internalServerError(String debugMessage, Throwable t) {
        noKeepalive();
        WebServer.LOG.FINE(t);
        if (!(t instanceof ClosedChannelException)) {
            if (t instanceof HandledException) {
                error(HttpResponseStatus.INTERNAL_SERVER_ERROR, (HandledException) t);
            } else {
                String requestUri = "?";
                if (wc != null && wc.getRequest() != null) {
                    requestUri = wc.getRequest().uri();
                }
                Exceptions.handle()
                          .to(WebServer.LOG)
                          .withSystemErrorMessage(
                                  "An excption occurred while responding to: %s - %s (%s) [Debug-Message: %s]",
                                  requestUri,
                                  debugMessage)
                          .handle();
                error(HttpResponseStatus.INTERNAL_SERVER_ERROR, Exceptions.handle(WebServer.LOG, t));
            }
        }
    }

    /*
     * Sets the Date and Cache headers for the HTTP Response
     */
    protected void setDateAndCacheHeaders(long lastModifiedMillis, int cacheSeconds, boolean isPrivate) {
        if (headers().contains(HttpHeaderNames.EXPIRES) || headers().contains(HttpHeaderNames.CACHE_CONTROL)) {
            return;
        }
        SimpleDateFormat formatter = getHTTPDateFormat();

        if (cacheSeconds > 0) {
            // Date header
            Calendar time = new GregorianCalendar();
            addHeaderIfNotExists(HttpHeaderNames.DATE, formatter.format(time.getTime()));

            // Add cached headers
            time.add(Calendar.SECOND, cacheSeconds);
            addHeaderIfNotExists(HttpHeaderNames.EXPIRES, formatter.format(time.getTime()));
            if (isPrivate) {
                addHeaderIfNotExists(HttpHeaderNames.CACHE_CONTROL, "private, max-age=" + cacheSeconds);
            } else {
                addHeaderIfNotExists(HttpHeaderNames.CACHE_CONTROL, "public, max-age=" + cacheSeconds);
            }
        } else {
            addHeaderIfNotExists(HttpHeaderNames.CACHE_CONTROL, HttpHeaderValues.NO_CACHE + ", max-age=0");
        }
        if (lastModifiedMillis > 0 && !headers().contains(HttpHeaderNames.LAST_MODIFIED)) {
            addHeaderIfNotExists(HttpHeaderNames.
                                         LAST_MODIFIED, formatter.format(new Date(lastModifiedMillis)));
        }
    }

    /*
     * Creates a DateFormat to parse HTTP dates.
     */
    protected SimpleDateFormat getHTTPDateFormat() {
        if (dateFormatter == null) {
            dateFormatter = new SimpleDateFormat(WebContext.HTTP_DATE_FORMAT, Locale.US);
            dateFormatter.setTimeZone(TIME_ZONE_GMT);
        }
        return dateFormatter;
    }

    /*
     * Sets the content disposition header for the HTTP Response
     */
    protected void setContentDisposition(String name, boolean download) {
        String cleanName = name.replaceAll("[^A-Za-z0-9\\-_.]", "_");
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
    protected void setContentTypeHeader(String name) {
        addHeaderIfNotExists(HttpHeaderNames.CONTENT_TYPE, MimeHelper.guessMimeType(name));
    }

    /**
     * Tries to resolve the given name into a {@link Resource} using
     * the {@link Resources} lookup framework.
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
            } catch (Exception e) {
                internalServerError("Content to send: " + name, e);
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
            addHeaderIfNotExists(HttpHeaderNames.CONTENT_LENGTH, fileLength);
            String contentType = MimeHelper.guessMimeType(name != null ? name : urlConnection.getURL().getFile());
            addHeaderIfNotExists(HttpHeaderNames.CONTENT_TYPE, contentType);
            setDateAndCacheHeaders(urlConnection.getLastModified(),
                                   cacheSeconds == null ? HTTP_CACHE : cacheSeconds,
                                   isPrivate);
            if (name != null) {
                setContentDisposition(name, download);
            }

            DefaultHttpResponse response = canBeCompressed(contentType) ?
                                           createChunkedResponse(HttpResponseStatus.OK, true) :
                                           createResponse(HttpResponseStatus.OK, true);
            commit(response);
            installChunkedWriteHandler();
            if (responseChunked) {
                ctx.write(new HttpChunkedInput(new ChunkedStream(urlConnection.getInputStream(), BUFFER_SIZE)));
                ChannelFuture writeFuture = ctx.writeAndFlush(Unpooled.EMPTY_BUFFER);
                complete(writeFuture);
            } else {
                ctx.write(new ChunkedStream(urlConnection.getInputStream(), BUFFER_SIZE));
                ChannelFuture writeFuture = ctx.writeAndFlush(LastHttpContent.EMPTY_LAST_CONTENT);
                complete(writeFuture);
            }
        } catch (Exception t) {
            internalServerError("Resource to send: " + urlConnection.getURL().toString(), t);
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
                ctx.channel().close();
                return;
            }
            if (HttpMethod.HEAD.equals(wc.getRequest().method()) || HttpResponseStatus.NOT_MODIFIED.equals(status)) {
                status(status);
                return;
            }

            renderErrorTemplate(status, message);
        } catch (Exception e) {
            handleErrorInError(status, message, e);
        }
    }

    private void renderErrorTemplate(HttpResponseStatus status, String message) {
        try {
            if (HttpResponseStatus.NOT_FOUND.equals(status)) {
                template(status, "templates/http/not-found.html.pasta", CallContext.getCurrent(), message);
            } else {
                String effectiveMessage = message;
                if (Strings.isEmpty(effectiveMessage)) {
                    effectiveMessage = status.toString();
                }
                template(status, "templates/http/error.html.pasta", CallContext.getCurrent(), effectiveMessage);
            }
        } catch (HandledException e) {
            Exceptions.ignore(e);
            template(status, "templates/http/plain-error.html.pasta", CallContext.getCurrent(), message);
        }
    }

    private void handleErrorInError(HttpResponseStatus status, String message, Exception e) {
        if (e instanceof ClosedChannelException) {
            return;
        }

        HandledException he = Exceptions.handle()
                                        .to(WebServer.LOG)
                                        .error(e)
                                        .withSystemErrorMessage("An exception occurred while sending an HTTP error! "
                                                                + "Original Status Code: %s, "
                                                                + "Original Error: %s, "
                                                                + "URL: %s - %s (%s)",
                                                                status == null ? "null" : status.code(),
                                                                message,
                                                                wc == null || wc.getRequest() == null ?
                                                                "?" :
                                                                wc.getRequest().uri())
                                        .handle();

        if (wc == null || wc.responseCommitted) {
            if (ctx.channel().isOpen()) {
                ctx.channel().close();
            }
            return;
        }
        if (!ctx.channel().isWritable()) {
            return;
        }

        ByteBuf channelBuffer = wrapUTF8String(he.getMessage());
        HttpResponse response = createFullResponse(HttpResponseStatus.INTERNAL_SERVER_ERROR, false, channelBuffer);
        response.headers().set(HttpHeaderNames.CONTENT_TYPE, "text/plain; charset=UTF-8");
        HttpUtil.setContentLength(response, channelBuffer.readableBytes());
        completeAndClose(commit(response));
    }

    /*
     * Converts a string into a ByteBuf
     */
    private ByteBuf wrapUTF8String(String content) {
        // Returns a heap buffer - but strings are almost always compressed (HTML templates etc.) so this
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
        } catch (Exception e) {
            internalServerError("Cannot send direct content", e);
        }
    }

    /**
     * Renders the given template and sends the output as response.
     * <p>
     * By default caching will be disabled. If the file ends with .html, <tt>text/html; charset=UTF-8</tt> will be set
     * as content type. Otherwise the content type will be guessed from the filename.
     *
     * @param name   the name of the template to render. It's recommended to use files in /templates/... and to place
     *               them in the resources directory.
     * @param params contains the parameters sent to the template
     */
    public void template(String name, Object... params) {
        template(HttpResponseStatus.OK, name, params);
    }

    /**
     * Renders the given template and sends the output as response.
     * <p>
     * By default caching will be disabled. If the file ends with .html, <tt>text/html; charset=UTF-8</tt> will be set
     * as content type. Otherwise the content type will be guessed from the filename.
     *
     * @param status the HTTP status to send. {@link HttpResponseStatus#OK} would be appropriate in most cases.
     * @param name   the name of the template to render. It's recommended to use files in /templates/... and to place
     *               them in the resources directory.
     * @param params contains the parameters sent to the template
     * @see #template(String, Object...)
     */
    public void template(HttpResponseStatus status, String name, Object... params) {
        try {
            Template template = engine.resolve(name)
                                      .orElseThrow(() -> Exceptions.handle()
                                                                   .to(Resources.LOG)
                                                                   .withSystemErrorMessage(
                                                                           "Cannot find the template: %s",
                                                                           name)
                                                                   .handle());
            template(status, template, params);
        } catch (CompileException e) {
            throw Exceptions.handle().to(Resources.LOG).error(e).withSystemErrorMessage("%s", e.getMessage()).handle();
        }
    }

    /**
     * Renders the given Rythm template and sends the output as response.
     * <p>
     * By default caching will be disabled. If the file ends with .html, <tt>text/html; charset=UTF-8</tt> will be set
     * as content type. Otherwise the content type will be guessed from the filename.
     *
     * @param status   the HTTP status to send. {@link HttpResponseStatus#OK} would be appropriate in most cases.
     * @param template the template to render
     * @param params   contains the parameters sent to the template
     * @see #template(HttpResponseStatus, String, Object...)
     */
    public void template(HttpResponseStatus status, Template template, Object... params) {
        wc.enableTiming(null);
        try {
            Object[] effectiveParams = fixParams(params);
            GlobalRenderContext renderContext = engine.createRenderContext();
            template.render(renderContext, effectiveParams);
            sendTemplateContent(status, template.getEffectiveFileName(), renderContext.toString());
        } catch (Exception e) {
            handleTemplateError(template.getEffectiveFileName(), e);
        }
    }

    private void setupContentType(Template template) {
        String fileName = template.getEffectiveFileName();
        if (fileName.endsWith(FILETYPE_HTML)) {
            setHeader(HttpHeaderNames.CONTENT_TYPE, CONTENT_TYPE_HTML);
        } else {
            setContentTypeHeader(fileName);
        }
    }

    private void handleTemplateError(String name, Exception e) {
        throw Exceptions.handle()
                        .to(Tagliatelle.LOG)
                        .error(e)
                        .withSystemErrorMessage("Failed to render the template '%s': %s (%s)", name)
                        .handle();
    }

    protected void sendTemplateContent(HttpResponseStatus status, String name, String content) {
        try {
            if (name.endsWith("html")) {
                setHeader(HttpHeaderNames.CONTENT_TYPE, CONTENT_TYPE_HTML);
            } else {
                setContentTypeHeader(name);
            }
            setDateAndCacheHeaders(System.currentTimeMillis(), cacheSeconds == null ? 0 : cacheSeconds, isPrivate);
            ByteBuf channelBuffer = wrapUTF8String(content);
            HttpResponse response = createFullResponse(status, true, channelBuffer);
            complete(commit(response));
        } catch (Exception e) {
            throw Exceptions.handle()
                            .to(WebServer.LOG)
                            .error(e)
                            .withSystemErrorMessage("Cannot send content of template: " + name)
                            .handle();
        }
    }

    private Object[] fixParams(Object[] params) {
        if (params.length == 1 && params[0] instanceof Object[]) {
            return (Object[]) params[0];
        }
        return params;
    }

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
        tunnel(url, null);
    }

    /**
     * Tunnels the contents retrieved from the given URL as result of this response.
     * <p>
     * Caching and range headers will be forwarded and adhered.
     * <p>
     * Uses non-blocking APIs in order to maximize throughput. Therefore this can be called in an unforked
     * dispatcher.
     * <p>
     * If the called URL returns an error (&gt;= 400) and the given failureHandler is non null, it is supplied
     * with the status code and can re-try or answer the request by itself.
     *
     * @param url            the url to tunnel through.
     * @param failureHandler supplies a handler which is invoked if the called URL fails. The handler is provided with
     *                       the HTTP status code and can (and must) handle the request on its own. It is save to
     *                       call {@link WebContext#respondWith()} again for the request, as no response was created
     *                       yet.
     */
    public void tunnel(final String url, @Nullable Consumer<Integer> failureHandler) {
        try {
            AsyncHttpClient.BoundRequestBuilder brb = getAsyncClient().prepareGet(url);
            long ifModifiedSince = wc.getDateHeader(HttpHeaderNames.IF_MODIFIED_SINCE);
            if (ifModifiedSince > 0) {
                brb.addHeader(HttpHeaderNames.IF_MODIFIED_SINCE.toString(),
                              getHTTPDateFormat().format(ifModifiedSince));
            }

            // Support range requests...
            String range = wc.getHeader(HttpHeaderNames.RANGE);
            if (Strings.isFilled(range)) {
                brb.addHeader(HttpHeaderNames.RANGE.toString(), range);
            }
            if (WebServer.LOG.isFINE()) {
                WebServer.LOG.FINE("Tunnel START: %s", url);
            }

            // Tunnel it through...
            brb.execute(new TunnelHandler(this, url, failureHandler));
        } catch (Exception t) {
            internalServerError("Target-URL: " + url, t);
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
     * By default, caching will be supported.
     *
     * @param status      the HTTP status to send
     * @param contentType the content type to use. If <tt>null</tt>, we rely on a previously set header.
     * @return an output stream which will be sent as response
     */
    public OutputStream outputStream(final HttpResponseStatus status, @Nullable final String contentType) {
        if (wc.responseCommitted) {
            throw Exceptions.createHandled()
                            .withSystemErrorMessage("Response for %s was already committed!", wc.getRequestedURI())
                            .handle();
        }

        return new ChunkedOutputStream(this, contentType, status);
    }

    /**
     * Writes the given message probably a chunk of output data into the channel.
     * <p>
     * If the channel buffer is full (not writeable anymore) we need to trigger a flush,
     * so that the data is shovelled into the network. If this doesn't clear up the buffer immediatelly,
     * we block the current thread to throttle the application until free space is available again.
     * <p>
     * Note that this method must not be invoked in the event loop as otherwise a deadlock might occur. Therefore
     * all dispatchers now always for a new thread to handle requests.
     *
     * @param message the data to sent
     */
    protected void contentionAwareWrite(Object message) {
        if (!ctx.channel().isWritable()) {
            ChannelFuture future = ctx.writeAndFlush(message);
            while (!ctx.channel().isWritable() && ctx.channel().isOpen()) {
                try {
                    future.await(5, TimeUnit.SECONDS);
                } catch (InterruptedException e) {
                    ctx.channel().close();
                    Exceptions.ignore(e);
                    Thread.currentThread().interrupt();
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
