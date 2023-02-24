/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.http;

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
import org.asynchttpclient.AsyncHttpClient;
import org.asynchttpclient.BoundRequestBuilder;
import org.asynchttpclient.Dsl;
import sirius.kernel.Sirius;
import sirius.kernel.async.CallContext;
import sirius.kernel.async.ExecutionPoint;
import sirius.kernel.commons.MultiMap;
import sirius.kernel.commons.Processor;
import sirius.kernel.commons.Strings;
import sirius.kernel.commons.Value;
import sirius.kernel.di.std.Part;
import sirius.kernel.health.Exceptions;
import sirius.kernel.health.HandledException;
import sirius.kernel.health.Microtiming;
import sirius.kernel.nls.NLS;
import sirius.kernel.xml.Outcall;
import sirius.kernel.xml.XMLStructuredOutput;
import sirius.pasta.Pasta;
import sirius.pasta.noodle.compiler.CompileException;
import sirius.pasta.tagliatelle.Tagliatelle;
import sirius.pasta.tagliatelle.Template;
import sirius.pasta.tagliatelle.rendering.GlobalRenderContext;
import sirius.web.controller.PreserveErrorMessageTransformer;
import sirius.web.resources.Resource;
import sirius.web.resources.Resources;
import sirius.web.services.JSONStructuredOutput;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.io.File;
import java.io.IOException;
import java.net.URLConnection;
import java.nio.channels.ClosedChannelException;
import java.nio.charset.StandardCharsets;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.Collection;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.function.Consumer;
import java.util.function.IntConsumer;
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

    /**
     * The name of the Cookie that enables debugging of rendered contents.
     */
    private static final String SIRIUS_DEBUG_COOKIE = "SIRIUS.WEB.DEBUG.LEVEL";

    /**
     * Contains a set of parameter names which are censored in any output as we do not want to log user passwords etc.
     */
    private static final Set<String> CENSORED_LOWERCASE_PARAMETER_NAMES =
            Set.of("password", "passphrase", "secret", "secretKey");

    /*
     * Contains the content type used for html
     */
    private static final String CONTENT_TYPE_HTML = "text/html; charset=UTF-8";

    /*
     * Represents a value to be used for CACHE_CONTROL which prevents any caching...
     */
    private static final String NO_CACHE = HttpHeaderValues.NO_CACHE + ", max-age=0";

    /*
     * Stores the associated request
     */
    protected WebContext webContext;

    /*
     * Stores the underlying channel
     */
    protected ChannelHandlerContext channelHandlerContext;

    /*
     * Stores the outgoing headers to be sent
     */
    private HttpHeaders headers;

    /*
     * Stores the effective response code.
     */
    private volatile int responseCode;

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
     * @param webContext the context representing the request for which this response is created
     */
    protected Response(WebContext webContext) {
        this.webContext = webContext;
        this.channelHandlerContext = webContext.getChannelHandlerContext();
    }

    /*
     * Creates and initializes a HttpResponse with a complete result at hands.
     * Takes care of the keep alive logic, cookies and other default headers
     */
    protected DefaultFullHttpResponse createFullResponse(HttpResponseStatus status, boolean keepalive, ByteBuf buffer) {
        DefaultFullHttpResponse response = new DefaultFullHttpResponse(HttpVersion.HTTP_1_1, status, buffer);
        setupResponse(status, keepalive, response);

        // The user may have manually set a content-length, for instance when
        // responding to a HEAD request. In such cases, we trust the user that
        // the value is correct and don't change it.
        if (!response.headers().contains(HttpHeaderNames.CONTENT_LENGTH)) {
            response.headers().set(HttpHeaderNames.CONTENT_LENGTH, buffer.readableBytes());
        }

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
     * Creates and initializes a HttpResponse which result will follow as chunks. If the requester does not
     * support HTTP 1.1 we fall back to a "normal" response and disable keepalive (as we need to close the
     * connection to signal the end of the response). Check the responseChunked flag to generate a proper
     * response.
     *
     * Takes care of the keep alive logic, cookies and other default headers
     */
    protected DefaultHttpResponse createChunkedResponse(HttpResponseStatus status, boolean keepalive) {
        if (HttpVersion.HTTP_1_0.equals(webContext.getRequest().protocolVersion())) {
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

        // Never cache any server-sided errors...
        if (status.code() >= 500) {
            response.headers().set(HttpHeaderNames.CACHE_CONTROL, NO_CACHE);
        }

        // Add keepalive header if required
        if (responseKeepalive && keepalive && isKeepalive()) {
            response.headers().set(HttpHeaderNames.CONNECTION, HttpHeaderValues.KEEP_ALIVE);
        } else {
            if (!HttpVersion.HTTP_1_0.equals(webContext.getRequest().protocolVersion())) {
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
        String requestedOrigin = webContext.getHeader(HttpHeaderNames.ORIGIN);
        if (Strings.isFilled(requestedOrigin)) {
            response.headers().set(HttpHeaderNames.ACCESS_CONTROL_ALLOW_ORIGIN, requestedOrigin);
            if (!response.headers().contains(HttpHeaderNames.ACCESS_CONTROL_ALLOW_CREDENTIALS)) {
                response.headers().set(HttpHeaderNames.ACCESS_CONTROL_ALLOW_CREDENTIALS, "true");
            }
        }
    }

    private void setupCookies(DefaultHttpResponse response) {
        Collection<Cookie> cookies = webContext.getOutCookies(isCacheable(response));
        if (cookies != null && !cookies.isEmpty()) {
            response.headers().set(HttpHeaderNames.SET_COOKIE, ServerCookieEncoder.LAX.encode(cookies));
        }
    }

    private boolean isCacheable(DefaultHttpResponse response) {
        // Check for a manually added expires header (e.g. due to tunneling)...
        if (response.headers().contains(HttpHeaderNames.EXPIRES)) {
            return true;
        }

        // Check for a manually added cache-control header (e.g. due to tunneling)...
        String cacheControl = response.headers().get(HttpHeaderNames.CACHE_CONTROL);
        return cacheControl != null && !cacheControl.startsWith(HttpHeaderValues.NO_CACHE.toString());
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
                LocalDateTime expires =
                        WebServer.parseDateHeader(response.headers().get(HttpHeaderNames.EXPIRES)).orElse(null);
                if (expires != null && LocalDateTime.now().isBefore(expires)) {
                    WebServer.LOG.WARN("A response with 'set-cookie' and 'expires' was created for URI: %s%n%s%n%s",
                                       webContext.getRequestedURI(),
                                       webContext,
                                       ExecutionPoint.snapshot());
                    response.headers().remove(HttpHeaderNames.EXPIRES);
                }
            }
            String cacheControl = response.headers().get(HttpHeaderNames.CACHE_CONTROL);
            if (cacheControl != null
                && !cacheControl.startsWith(HttpHeaderValues.NO_CACHE.toString())
                && !cacheControl.startsWith(HttpHeaderValues.MUST_REVALIDATE.toString())
                && !cacheControl.startsWith(HttpHeaderValues.NO_STORE.toString())) {
                WebServer.LOG.WARN("A response with 'set-cookie' and 'cache-control' was created for URI: %s%n%s%n%s",
                                   webContext.getRequestedURI(),
                                   webContext,
                                   ExecutionPoint.snapshot());
                response.headers().set(HttpHeaderNames.CACHE_CONTROL, NO_CACHE);
            }
        }
    }

    private void updateStatistics(HttpResponseStatus status) {
        if (status.code() >= 500) {
            if (WebServer.serverErrors.incrementAndGet() < 0) {
                WebServer.serverErrors.set(0);
            }
        } else if (status.code() >= 400 && WebServer.clientErrors.incrementAndGet() < 0) {
            WebServer.clientErrors.set(0);
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
        if (webContext.responseCommitted) {
            if (response instanceof FullHttpResponse fullHttpResponse) {
                fullHttpResponse.release();
            }
            throw Exceptions.handle()
                            .to(WebServer.LOG)
                            .error(new IllegalStateException())
                            .withSystemErrorMessage("Response for %s was already committed!",
                                                    webContext.getRequestedURI())
                            .handle();
        }
        if (WebServer.LOG.isFINE()) {
            WebServer.LOG.FINE("COMMITTING: " + webContext.getRequestedURI());
        }

        // If the request has not been fully read, now is the time to discard all
        // data, as most HTTP clients do not accept a response while uploading data.
        // -> This mostly happened when handling an exception in a pre-dispatchable
        // controller...
        if (webContext.contentHandler != null) {
            webContext.contentHandler.exhaust();
        }

        responseCode = response.status().code();
        webContext.responseCommitted = true;
        webContext.committed = System.currentTimeMillis();
        webContext.releaseContentHandler();
        return flush ? channelHandlerContext.writeAndFlush(response) : channelHandlerContext.write(response);
    }

    /**
     * Provides access to the web context for which this response was created.
     *
     * @return the HTTP request for which this response was created
     */
    public WebContext getWebContext() {
        return webContext;
    }

    /**
     * Provides access to the channel handler context.
     *
     * @return the channel handler context
     */
    protected ChannelHandlerContext getChannelHandlerContext() {
        return channelHandlerContext;
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
        return HttpUtil.isKeepAlive(webContext.getRequest())
               && ((WebServerHandler) channelHandlerContext.handler()).shouldKeepAlive();
    }

    /*
     * Completes the response and closes the underlying channel if necessary
     */
    private void complete(ChannelFuture future, final boolean supportKeepalive) {
        if (webContext.responseCompleted) {
            WebServer.LOG.FINE("Response for %s was already completed!", webContext.getRequestedURI());
            return;
        }
        webContext.responseCompleted = true;
        if (webContext.completionPromise != null) {
            webContext.completionPromise.success(responseCode);
        }
        if (WebServer.LOG.isFINE()) {
            WebServer.LOG.FINE("COMPLETING: " + webContext.getRequestedURI());
        }
        // If we're still confident, that keepalive is supported, and we announced this in the response header,
        // we'll keep the connection open. Otherwise, it will be closed by the server
        final boolean keepalive = supportKeepalive && responseKeepalive;
        final CallContext cc = CallContext.getCurrent();
        future.addListener(ignored -> onCompleteCompleted(cc, keepalive, future));
    }

    private void onCompleteCompleted(CallContext callContext, boolean keepalive, ChannelFuture future) {
        if (webContext.completionCallback != null) {
            try {
                webContext.completionCallback.invoke(callContext);
            } catch (Exception e) {
                Exceptions.handle(WebServer.LOG, e);
            }
        }
        webContext.release();
        updateResponseTimeMetrics(callContext);
        handleKeepalive(keepalive, future);
    }

    private void updateResponseTimeMetrics(CallContext callContext) {
        if (webContext.microtimingKey != null && Microtiming.isEnabled()) {
            callContext.getWatch().submitMicroTiming("HTTP", WebServer.microtimingMode.getMicrotimingKey(webContext));
        }
        if (webContext.isLongCall() || webContext.scheduled == 0) {
            // No response time measurement for long-running or aborted requests...
            return;
        }
        long queuedMillis = webContext.scheduled - webContext.started;
        long ttfbMillis = webContext.getTTFBMillis();
        long responseTimeMillis = System.currentTimeMillis() - webContext.started;

        WebServer.queueTime.addValue(queuedMillis);
        WebServer.timeToFirstByte.addValue(ttfbMillis);
        WebServer.responseTime.addValue(responseTimeMillis);

        if (ttfbMillis > WebServer.getMaxTimeToFirstByte() && WebServer.getMaxTimeToFirstByte() > 0) {
            if (WebServer.slowRequests.incrementAndGet() < 0) {
                WebServer.slowRequests.set(0);
            }
            WebServer.LOG.WARN("Long running request: %s (Response Time: %s, Queue Time: %s, TTFB: %s)"
                               + "%nURL:%s"
                               + "%nParameters:"
                               + "%n%s"
                               + "%nMDC:"
                               + "%n%s%n",
                               webContext.getRequestedURI(),
                               NLS.convertDuration(responseTimeMillis, true, true),
                               NLS.convertDuration(queuedMillis, true, true),
                               NLS.convertDuration(ttfbMillis, true, true),
                               webContext.getRequestedURL(),
                               webContext.getParameterNames()
                                         .stream()
                                         .map(param -> param + ": " + censor(param))
                                         .collect(Collectors.joining("\n")),
                               callContext);
        }
    }

    @Nonnull
    private String censor(@Nonnull String parameterName) {
        if (CENSORED_LOWERCASE_PARAMETER_NAMES.contains(parameterName.toLowerCase())) {
            return "(censored)";
        } else {
            return Strings.limit(webContext.get(parameterName).asString(), 50);
        }
    }

    private void handleKeepalive(boolean keepalive, ChannelFuture future) {
        if (!keepalive) {
            if (WebServer.LOG.isFINE()) {
                WebServer.LOG.FINE("CLOSING: " + webContext.getRequestedURI());
            }
            future.channel().close();
        } else {
            if (WebServer.LOG.isFINE()) {
                WebServer.LOG.FINE("KEEP-ALIVE: " + webContext.getRequestedURI());
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
        long ifModifiedSinceDateSeconds = WebServer.parseDateHeader(getHeader(HttpHeaderNames.IF_MODIFIED_SINCE))
                                                   .map(date -> date.atZone(ZoneId.systemDefault())
                                                                    .toInstant()
                                                                    .getEpochSecond())
                                                   .orElse(0L);
        if (ifModifiedSinceDateSeconds > 0
            && lastModifiedInMillis > 0
            && ifModifiedSinceDateSeconds >= lastModifiedInMillis / 1000) {
            setDateAndCacheHeaders(lastModifiedInMillis, cacheSeconds == null ? HTTP_CACHE : cacheSeconds, isPrivate);
            status(HttpResponseStatus.NOT_MODIFIED);
            return true;
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
     * Marks this response as not-cacheable.
     *
     * @return <tt>this</tt> to fluently create the response
     */
    public Response notCached() {
        this.cacheSeconds = 0;
        return this;
    }

    /**
     * Marks this response as only privately cacheable (only the browser may cache it, but not a proxy etc.)
     *
     * @return <tt>this</tt> to fluently create the response
     */
    public Response privateCached() {
        this.isPrivate = true;
        this.cacheSeconds = HTTP_CACHE;
        return this;
    }

    /**
     * Marks this response as cacheable for the given amount of time.
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
     * Marks this response as cacheable.
     *
     * @return <tt>this</tt> to fluently create the response
     */
    public Response cached() {
        this.isPrivate = false;
        this.cacheSeconds = HTTP_CACHE;
        return this;
    }

    /**
     * Marks this response as infinitely cacheable.
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
     * Returns the value of a header with the specified name. If there are
     * more than one values for the specified name, the first value is returned.
     *
     * @param name The name of the header to search
     * @return The first header value or {@code null} if there is no such header
     * @see io.netty.handler.codec.http.HttpHeaders#get(java.lang.CharSequence)
     */
    @Nullable
    public String getHeader(CharSequence name) {
        return headers().get(name);
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
        if (HttpVersion.HTTP_1_0.equals(webContext.getRequest().protocolVersion())) {
            // Fallback to HTTP/1.0 code 302 found, which does mostly the same job but has a bad image due to
            // URL hijacking via faulty search engines. The main difference is that 307 will enforce the browser
            // to use the same method for the request to the reported location. Whereas 302 doesn't specify which
            // method to use, so a POST might be re-sent as GET to the new location
            redirectToGet(url);
        } else {
            // Prefer the HTTP/1.1 code 307 as temporary redirect
            performRedirect(url, HttpResponseStatus.TEMPORARY_REDIRECT);
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
        performRedirect(url, HttpResponseStatus.FOUND);
    }

    /**
     * Sends a 301 (permanent redirect) to the given url as result.
     *
     * @param url the URL to redirect to
     */
    public void redirectPermanently(String url) {
        performRedirect(url, HttpResponseStatus.MOVED_PERMANENTLY);
    }

    private void performRedirect(String url, HttpResponseStatus status) {
        if (cacheSeconds == null || cacheSeconds == 0) {
            userMessagesCache.cacheUserMessages(webContext);
        } else {
            setDateAndCacheHeaders(System.currentTimeMillis(), cacheSeconds, isPrivate);
        }

        HttpResponse response = createFullResponse(status, true, Unpooled.EMPTY_BUFFER);
        response.headers().set(HttpHeaderNames.LOCATION, url);
        complete(commit(response));
    }

    /*
     * Determines if the current request should be compressed or not
     */
    protected boolean canBeCompressed(String contentType) {
        String acceptEncoding = webContext.getRequest().headers().get(HttpHeaderNames.ACCEPT_ENCODING);
        if (acceptEncoding == null || (!acceptEncoding.contains(HttpHeaderValues.GZIP) && !acceptEncoding.contains(
                HttpHeaderValues.DEFLATE))) {
            return false;
        }
        return MimeHelper.isCompressable(contentType);
    }

    protected void installChunkedWriteHandler() {
        if (channelHandlerContext.channel().pipeline().get(ChunkedWriteHandler.class) == null
            && channelHandlerContext.channel().isOpen()) {
            channelHandlerContext.channel().pipeline().addBefore("handler", "chunkedWriter", new ChunkedWriteHandler());
        }
    }

    protected void removedChunkedWriteHandler(ChannelFuture writeFuture) {
        writeFuture.addListener(ignored -> {
            if (channelHandlerContext.channel().pipeline().get(ChunkedWriteHandler.class) != null
                && channelHandlerContext.channel().isOpen()) {
                channelHandlerContext.pipeline().remove(ChunkedWriteHandler.class);
            }
        });
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
            if (t instanceof HandledException handledException) {
                error(HttpResponseStatus.INTERNAL_SERVER_ERROR, handledException);
            } else {
                String requestUri = "?";
                if (webContext != null && webContext.getRequest() != null) {
                    requestUri = webContext.getRequest().uri();
                }
                Exceptions.handle()
                          .to(WebServer.LOG)
                          .withSystemErrorMessage(
                                  "An exception occurred while responding to: %s - %s (%s) [Debug-Message: %s]",
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

        if (cacheSeconds > 0) {
            // Date header
            addHeaderIfNotExists(HttpHeaderNames.DATE,
                                 Outcall.RFC2616_INSTANT.format(LocalDateTime.now().atZone(ZoneId.systemDefault())));

            // Add cached headers
            addHeaderIfNotExists(HttpHeaderNames.EXPIRES,
                                 Outcall.RFC2616_INSTANT.format(LocalDateTime.now()
                                                                             .atZone(ZoneId.systemDefault())
                                                                             .plusSeconds(cacheSeconds)));
            if (isPrivate) {
                addHeaderIfNotExists(HttpHeaderNames.CACHE_CONTROL, "private, max-age=" + cacheSeconds);
            } else {
                addHeaderIfNotExists(HttpHeaderNames.CACHE_CONTROL, "public, max-age=" + cacheSeconds);
            }
        } else {
            addHeaderIfNotExists(HttpHeaderNames.CACHE_CONTROL, NO_CACHE);
        }
        if (lastModifiedMillis > 0 && !headers().contains(HttpHeaderNames.LAST_MODIFIED)) {
            addHeaderIfNotExists(HttpHeaderNames.LAST_MODIFIED,
                                 Outcall.RFC2616_INSTANT.format(Instant.ofEpochMilli(lastModifiedMillis)
                                                                       .atZone(ZoneId.systemDefault())));
        }
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
                channelHandlerContext.write(new HttpChunkedInput(new ChunkedStream(urlConnection.getInputStream(),
                                                                                   BUFFER_SIZE)));
                ChannelFuture writeFuture = channelHandlerContext.writeAndFlush(Unpooled.EMPTY_BUFFER);
                removedChunkedWriteHandler(writeFuture);
                complete(writeFuture);
            } else {
                channelHandlerContext.write(new ChunkedStream(urlConnection.getInputStream(), BUFFER_SIZE));
                ChannelFuture writeFuture = channelHandlerContext.writeAndFlush(LastHttpContent.EMPTY_LAST_CONTENT);
                removedChunkedWriteHandler(writeFuture);
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
            if (webContext.responseCommitted) {
                if (channelHandlerContext.channel().isOpen()) {
                    channelHandlerContext.channel().close();
                }
                return;
            }
            if (!channelHandlerContext.channel().isWritable()) {
                channelHandlerContext.channel().close();
                return;
            }
            if (HttpMethod.HEAD.equals(webContext.getRequest().method()) || HttpResponseStatus.NOT_MODIFIED.equals(
                    status)) {
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
                template(status, "/templates/http/not-found.html.pasta", CallContext.getCurrent(), message);
            } else {
                String effectiveMessage = message;
                if (Strings.isEmpty(effectiveMessage)) {
                    effectiveMessage = status.toString();
                }
                template(status,
                         "/templates/http/error.html.pasta",
                         CallContext.getCurrent(),
                         effectiveMessage,
                         status.reasonPhrase(),
                         status.code());
            }
        } catch (HandledException e) {
            Exceptions.ignore(e);
            template(status, "/templates/http/plain-error.html.pasta", CallContext.getCurrent(), message);
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
                                                                webContext == null || webContext.getRequest() == null ?
                                                                "?" :
                                                                webContext.getRequest().uri())
                                        .handle();

        if (webContext == null || webContext.responseCommitted) {
            if (channelHandlerContext.channel().isOpen()) {
                channelHandlerContext.channel().close();
            }
            return;
        }
        if (!channelHandlerContext.channel().isWritable()) {
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
        return Unpooled.copiedBuffer(content.toCharArray(), StandardCharsets.UTF_8);
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
     * By default, caching will be disabled. If the file ends with .html, <tt>text/html; charset=UTF-8</tt> will be set
     * as content type. Otherwise, the content type will be guessed from the filename.
     *
     * @param name   the name of the template to render. It's recommended to use files in /templates/... and to place
     *               them in the 'resources' directory.
     * @param params contains the parameters sent to the template
     */
    public void template(String name, Object... params) {
        template(HttpResponseStatus.OK, name, params);
    }

    /**
     * Renders the given template and sends the output as response.
     * <p>
     * By default, caching will be disabled. If the file ends with .html, <tt>text/html; charset=UTF-8</tt> will be set
     * as content type. Otherwise, the content type will be guessed from the filename.
     *
     * @param status the HTTP status to send. {@link HttpResponseStatus#OK} would be appropriate in most cases.
     * @param name   the name of the template to render. It's recommended to use files in /templates/... and to place
     *               them in the 'resources' directory.
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
            throw Exceptions.handle()
                            .to(Resources.LOG)
                            .hint(PreserveErrorMessageTransformer.PRESERVE, true)
                            .error(e)
                            .withDirectMessage(e.getMessage())
                            .handle();
        }
    }

    /**
     * Renders the given Tagliatelle template and sends the output as response.
     * <p>
     * By default, caching will be disabled. If the file ends with .html, <tt>text/html; charset=UTF-8</tt> will be set
     * as content type. Otherwise, the content type will be guessed from the filename.
     *
     * @param status   the HTTP status to send. {@link HttpResponseStatus#OK} would be appropriate in most cases.
     * @param template the template to render
     * @param params   contains the parameters sent to the template
     * @see #template(HttpResponseStatus, String, Object...)
     */
    public void template(HttpResponseStatus status, Template template, Object... params) {
        webContext.enableTiming(null);
        try {
            Object[] effectiveParams = fixParams(params);
            GlobalRenderContext renderContext = engine.createRenderContext();
            renderContext.setDebugLevel(fetchDebugLevel());
            template.render(renderContext, effectiveParams);
            sendTemplateContent(status, template.getEffectiveFileName(), renderContext.toString());
        } catch (Exception e) {
            handleTemplateError(template.getEffectiveFileName(), e);
        }
    }

    private void handleTemplateError(String name, Exception e) {
        throw Exceptions.handle()
                        .to(Pasta.LOG)
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
        if (params.length == 1 && (params[0] instanceof Object[] objects)) {
            return objects;
        }
        return params;
    }

    /*
     * Generates and returns a pooling fully asynchronous HTTP client
     */
    protected static AsyncHttpClient getAsyncClient() {
        if (asyncClient == null) {
            asyncClient = Dsl.asyncHttpClient(Dsl.config().setCookieStore(null).setRequestTimeout(-1));
        }
        return asyncClient;
    }

    /*
     * Closes the async client used to tunnel data (if one was created).
     */
    protected static void closeAsyncClient() {
        if (asyncClient != null) {
            try {
                asyncClient.close();
            } catch (IOException e) {
                Exceptions.handle(WebServer.LOG, e);
            }
        }
    }

    /**
     * Tunnels the contents retrieved from the given URL as result of this response.
     * <p>
     * Caching and range headers will be forwarded and adhered.
     * <p>
     * Uses non-blocking APIs in order to maximize throughput. Therefore, this can be called in an un-forked
     * dispatcher.
     *
     * @param url the url to tunnel through.
     * @see #tunnel(String, Consumer, Processor, IntConsumer, boolean)
     * @see #tunnel(String, Consumer, Processor, IntConsumer)
     * @see #tunnel(String, Processor, IntConsumer)
     * @see #tunnel(String, IntConsumer)
     */
    public void tunnel(String url) {
        tunnel(url, null, null, null, false);
    }

    /**
     * Tunnels the contents retrieved from the given URL as result of this response.
     * <p>
     * Caching and range headers will be forwarded and adhered.
     * <p>
     * Uses non-blocking APIs in order to maximize throughput. Therefore, this can be called in an un-forked
     * dispatcher.
     * <p>
     * If the called URL returns an error (&gt;= 400) and the given failureHandler is non-null, it is supplied
     * with the status code and can re-try or answer the request by itself.
     *
     * @param url            the url to tunnel through.
     * @param failureHandler supplies a handler which is invoked if the called URL fails. The handler is provided with
     *                       the HTTP status code and can (and must) handle the request on its own. It is safe to
     *                       call {@link WebContext#respondWith()} again for the request, as no response was created
     *                       yet.
     * @see #tunnel(String, Consumer, Processor, IntConsumer, boolean)
     * @see #tunnel(String, Consumer, Processor, IntConsumer)
     * @see #tunnel(String, Processor, IntConsumer)
     * @see #tunnel(String)
     */
    public void tunnel(String url, @Nullable IntConsumer failureHandler) {
        tunnel(url, null, null, failureHandler, false);
    }

    /**
     * Tunnels the contents retrieved from the given URL as result of this response.
     * <p>
     * Caching and range headers will be forwarded and adhered.
     * <p>
     * Uses non-blocking APIs in order to maximize throughput. Therefore, this can be called in an un-forked
     * dispatcher.
     * <p>
     * If the called URL returns an error (&gt;= 400) and the given failureHandler is non-null, it is supplied
     * with the status code and can re-try or answer the request by itself.
     *
     * @param url            the url to tunnel through
     * @param transformer    the transformer which map / transforms the byte blocks being tunnelled. Note that once
     *                       all data has been processed an empty buffer is sent to signalize the end of the processing.
     * @param failureHandler supplies a handler which is invoked if the called URL fails. The handler is provided with
     *                       the HTTP status code and can (and must) handle the request on its own. It is safe to
     *                       call {@link WebContext#respondWith()} again for the request, as no response was created
     *                       yet.
     * @see #tunnel(String, Consumer, Processor, IntConsumer, boolean)
     * @see #tunnel(String, Consumer, Processor, IntConsumer)
     * @see #tunnel(String, IntConsumer)
     * @see #tunnel(String)
     */
    public void tunnel(String url,
                       @Nullable Processor<ByteBuf, Optional<ByteBuf>> transformer,
                       @Nullable IntConsumer failureHandler) {
        tunnel(url, null, transformer, failureHandler, false);
    }

    /**
     * Tunnels the contents retrieved from the given URL as result of this response.
     * <p>
     * Caching and range headers will be forwarded and adhered.
     * <p>
     * Uses non-blocking APIs in order to maximize throughput. Therefore, this can be called in an un-forked
     * dispatcher.
     * <p>
     * If the called URL returns an error (&gt;= 400) and the given failureHandler is non-null, it is supplied
     * with the status code and can re-try or answer the request by itself.
     *
     * @param url            the url to tunnel through
     * @param requestTuner   a callback which can enhance the request being sent to the upstream server (e.g. make
     *                       it a POST request or add additional headers).
     * @param transformer    the transformer which map / transforms the byte blocks being tunnelled. Note that once
     *                       all data has been processed an empty buffer is sent to signalize the end of the processing.
     * @param failureHandler supplies a handler which is invoked if the called URL fails. The handler is provided with
     *                       the HTTP status code and can (and must) handle the request on its own. It is safe to
     *                       call {@link WebContext#respondWith()} again for the request, as no response was created
     *                       yet.
     * @see #tunnel(String, Consumer, Processor, IntConsumer, boolean)
     * @see #tunnel(String, Processor, IntConsumer)
     * @see #tunnel(String, IntConsumer)
     * @see #tunnel(String)
     */
    public void tunnel(String url,
                       @Nullable Consumer<BoundRequestBuilder> requestTuner,
                       @Nullable Processor<ByteBuf, Optional<ByteBuf>> transformer,
                       @Nullable IntConsumer failureHandler) {
        tunnel(url, requestTuner, transformer, failureHandler, false);
    }

    /**
     * Tunnels the contents retrieved from the given URL as result of this response.
     * <p>
     * Caching and range headers will be forwarded and adhered.
     * <p>
     * Uses non-blocking APIs in order to maximize throughput. Therefore, this can be called in an un-forked
     * dispatcher.
     * <p>
     * If the called URL returns an error (&gt;= 400) and the given failureHandler is non-null, it is supplied
     * with the status code and can re-try or answer the request by itself.
     *
     * @param url                  the url to tunnel through
     * @param requestTuner         a callback which can enhance the request being sent to the upstream server (e.g. make
     *                             it a POST request or add additional headers).
     * @param transformer          the transformer which map / transforms the byte blocks being tunnelled. Note that
     *                             once all data has been processed an empty buffer is sent to signalize the end of the
     *                             processing.
     * @param failureHandler       supplies a handler which is invoked if the called URL fails. The handler is provided
     *                             with the HTTP status code and can (and must) handle the request on its own. It is
     *                             safe to call {@link WebContext#respondWith()} again for the request, as no response
     *                             was created yet.
     * @param forceContentDownload forces the content-disposition header set in this object. If forceContentDownload is
     *                             false, the header from the tunneled response may override this header
     * @see #tunnel(String, Consumer, Processor, IntConsumer)
     * @see #tunnel(String, Processor, IntConsumer)
     * @see #tunnel(String, IntConsumer)
     * @see #tunnel(String)
     */
    public void tunnel(String url,
                       @Nullable Consumer<BoundRequestBuilder> requestTuner,
                       @Nullable Processor<ByteBuf, Optional<ByteBuf>> transformer,
                       @Nullable IntConsumer failureHandler,
                       boolean forceContentDownload) {
        tunnel(url, requestTuner, transformer, failureHandler, null, forceContentDownload);
    }

    /**
     * Tunnels the contents retrieved from the given URL as result of this response.
     * <p>
     * Caching and range headers will be forwarded and adhered.
     * <p>
     * Uses non-blocking APIs in order to maximize throughput. Therefore, this can be called in an un-forked
     * dispatcher.
     * <p>
     * If the called URL returns an error (&gt;= 400) and the given failureHandler is non-null, it is supplied
     * with the status code and can re-try or answer the request by itself.
     *
     * @param url                  the url to tunnel through
     * @param requestTuner         a callback which can enhance the request being sent to the upstream server (e.g. make
     *                             it a POST request or add additional headers).
     * @param transformer          the transformer which map / transforms the byte blocks being tunnelled. Note that
     *                             once all data has been processed an empty buffer is sent to signalize the end of the
     *                             processing.
     * @param failureHandler       supplies a handler which is invoked if the called URL fails. The handler is provided
     *                             with the HTTP status code and can (and must) handle the request on its own. It is
     *                             safe to call {@link WebContext#respondWith()} again for the request, as no response
     *                             was created yet.
     * @param completionHandler    supplies a handler which is invoked on tunneling completion. If the called URL fails,
     *                             the failureHandler is invoked first, followed by the completionHandler. The handler
     *                             is provided with the {@link TunnelHandler}, which allows accessing timings and
     *                             status code.
     * @param forceContentDownload forces the content-disposition header set in this object. If forceContentDownload is
     *                             false, the header from the tunneled response may override this header
     * @see #tunnel(String, Consumer, Processor, IntConsumer)
     * @see #tunnel(String, Processor, IntConsumer)
     * @see #tunnel(String, IntConsumer)
     * @see #tunnel(String)
     */
    public void tunnel(String url,
                       @Nullable Consumer<BoundRequestBuilder> requestTuner,
                       @Nullable Processor<ByteBuf, Optional<ByteBuf>> transformer,
                       @Nullable IntConsumer failureHandler,
                       @Nullable Consumer<TunnelHandler> completionHandler,
                       boolean forceContentDownload) {
        try {
            BoundRequestBuilder brb = getAsyncClient().prepareGet(url);

            // Adds support for detecting stale cache contents via if-modified-since...
            WebServer.parseDateHeader(webContext.getHeader(HttpHeaderNames.IF_MODIFIED_SINCE))
                     .ifPresent(ifModifiedSince -> brb.addHeader(HttpHeaderNames.IF_MODIFIED_SINCE.toString(),
                                                                 ifModifiedSince.atZone(ZoneId.systemDefault())
                                                                                .format(Outcall.RFC2616_INSTANT)));

            // Support range requests...
            String range = webContext.getHeader(HttpHeaderNames.RANGE);
            if (Strings.isFilled(range)) {
                brb.addHeader(HttpHeaderNames.RANGE.toString(), range);
            }

            // Fine tune request if necessary...
            if (requestTuner != null) {
                requestTuner.accept(brb);
            }

            if (WebServer.LOG.isFINE()) {
                WebServer.LOG.FINE("Tunnel START: %s", url);
            }

            if (forceContentDownload && name != null) {
                setContentDisposition(name, download);
            }

            // Tunnel it through...
            brb.execute(new TunnelHandler(this, url, transformer, failureHandler, completionHandler));
        } catch (Exception t) {
            internalServerError("Target-URL: " + url, t);
        }
    }

    /**
     * Creates a JSON output which can be used to generate well-formed json.
     * <p>
     * By default, caching will be disabled. If the generated JSON is small enough, it will be transmitted in
     * one go. Otherwise, a chunked response will be sent.
     * <p>
     * If a callback parameter is given in the request, the output will automatically be boxed into that function as JSONP.
     *
     * @return a structured output which will be sent as JSON response
     */
    public JSONStructuredOutput json() {
        return json(HttpResponseStatus.OK);
    }

    /**
     * Creates a JSON output which can be used to generate well-formed json.
     * <p>
     * By default, caching will be disabled. If the generated JSON is small enough, it will be transmitted in
     * one go. Otherwise, a chunked response will be sent. The response will have the given {@link HttpResponseStatus}.
     * <p>
     * If a callback parameter is given in the request, the output will automatically be boxed into that function as JSONP.
     *
     * @param status the {@link HttpResponseStatus} the response shall have.
     * @return a structured output which will be sent as JSON response
     */
    public JSONStructuredOutput json(HttpResponseStatus status) {
        String callback = webContext.get("callback").getString();
        String encoding = webContext.get("encoding").first().asString(StandardCharsets.UTF_8.name());
        String mimeType = Strings.isFilled(callback) ? "application/javascript" : MimeHelper.APPLICATION_JSON;
        return new JSONStructuredOutput(outputStream(status, mimeType + ";charset=" + encoding), callback, encoding);
    }

    /**
     * Creates a XML output which can be used to generate well-formed XML.
     * <p>
     * By default, caching will be disabled. If the generated XML is small enough, it will be transmitted in
     * one go. Otherwise, a chunked response will be sent.
     *
     * @return a structured output which will be sent as XML response
     */
    public XMLStructuredOutput xml() {
        return xml(HttpResponseStatus.OK);
    }

    /**
     * Creates a XML output which can be used to generate well-formed XML.
     * <p>
     * By default, caching will be disabled. If the generated XML is small enough, it will be transmitted in
     * one go. Otherwise, a chunked response will be sent.
     *
     * @param status the {@link HttpResponseStatus} the response shall have.
     * @return a structured output which will be sent as XML response
     */
    public XMLStructuredOutput xml(HttpResponseStatus status) {
        return new XMLStructuredOutput(outputStream(status, MimeHelper.TEXT_XML));
    }

    /**
     * Creates an OutputStream which is sent to the client.
     * <p>
     * If the contents are small enough, everything will be sent in one response. Otherwise, a chunked response
     * will be sent. The size of the underlying buffer will be determined by {@link #BUFFER_SIZE}.
     * <p>
     * By default, caching will be supported.
     *
     * @param status      the HTTP status to send
     * @param contentType the content type to use. If <tt>null</tt>, we rely on a previously set header.
     * @return an output stream which will be sent as response
     */
    public ChunkedOutputStream outputStream(HttpResponseStatus status, @Nullable String contentType) {
        if (webContext.responseCommitted) {
            throw Exceptions.createHandled()
                            .withSystemErrorMessage("Response for %s was already committed!",
                                                    webContext.getRequestedURI())
                            .handle();
        }

        if (Strings.isEmpty(contentType) && Strings.isFilled(name)) {
            contentType = MimeHelper.guessMimeType(name);
        }

        return new ChunkedOutputStream(this, contentType, status);
    }

    @Override
    public String toString() {
        return "Response for: " + webContext.toString();
    }

    private GlobalRenderContext.DebugLevel fetchDebugLevel() {
        return Optional.ofNullable(webContext.getCookie(SIRIUS_DEBUG_COOKIE))
                       .map(cookie -> Value.of(cookie.value().toUpperCase())
                                           .asEnum(GlobalRenderContext.DebugLevel.class))
                       .orElse(GlobalRenderContext.DebugLevel.OFF);
    }
}
