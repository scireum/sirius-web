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
import io.netty.channel.ChannelHandlerContext;
import io.netty.channel.DefaultFileRegion;
import io.netty.handler.codec.http.DefaultFullHttpResponse;
import io.netty.handler.codec.http.DefaultHttpContent;
import io.netty.handler.codec.http.DefaultHttpHeaders;
import io.netty.handler.codec.http.DefaultHttpResponse;
import io.netty.handler.codec.http.DefaultLastHttpContent;
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
import java.util.function.Consumer;
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
    private static final String FILETYPE_HTML = ".html";

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

    protected static AsyncHttpClient asyncClient;

    private static final Pattern RANGE_HEADER = Pattern.compile("bytes=(\\d+)?-(\\d+)?");

    private static final Set<String> NON_TUNNELLED_HEADERS =
            Sets.newHashSet(HttpHeaderNames.TRANSFER_ENCODING.toString(),
                            HttpHeaderNames.SERVER.toString(),
                            HttpHeaderNames.CONTENT_ENCODING.toString(),
                            HttpHeaderNames.EXPIRES.toString(),
                            HttpHeaderNames.CACHE_CONTROL.toString());

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
        response.headers().set(HttpHeaderNames.CONTENT_LENGTH, buffer.readableBytes());
        return response;
    }

    /*
     * Creates and initializes a HttpResponse which result will follow as byte buffers
     * Takes care of the keep alive logic, cookies and other default headers
     */
    private DefaultHttpResponse createResponse(HttpResponseStatus status, boolean keepalive) {
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
    private DefaultHttpResponse createChunkedResponse(HttpResponseStatus status, boolean keepalive) {
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

        String requestedOrigin = wc.getHeader(HttpHeaderNames.ORIGIN);
        if (Strings.isFilled(requestedOrigin)) {
            response.headers().set(HttpHeaderNames.ACCESS_CONTROL_ALLOW_ORIGIN, requestedOrigin);
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

        // Adds a Strict Transport Security (HSTS) header...
        if (WebContext.forceHSTS) {
            response.headers()
                    .set("Strict-Transport-Security", "max-age=" + WebContext.hstsMaxAge + "; includeSubDomains");
        }
    }

    private void updateStatistics(HttpResponseStatus status) {
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
        if (wc.microtimingKey != null && Microtiming.isEnabled()) {
            callContext.getWatch().submitMicroTiming("HTTP", WebServer.microtimingMode.getMicrotimingKey(wc));
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
        HttpResponse response = createFullResponse(HttpResponseStatus.MOVED_PERMANENTLY, true, Unpooled.EMPTY_BUFFER);
        response.headers().set(HttpHeaderNames.LOCATION, url);
        complete(commit(response));
    }

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

                if (handleIfModifiedSince(file.lastModified())) {
                    return;
                }

                raf = new RandomAccessFile(file, "r");
                setDateAndCacheHeaders(file.lastModified(),
                                       cacheSeconds == null ? HTTP_CACHE : cacheSeconds,
                                       isPrivate);

                if (!parseRangesAndUpdateHeaders()) {
                    error(HttpResponseStatus.REQUESTED_RANGE_NOT_SATISFIABLE, "Cannot parse 'accept-ranges'.");
                    return;
                }

                if (name != null) {
                    setContentDisposition(name, download);
                }
                sendFileResponse();
            } catch (Exception e) {
                internalServerError("File: " + file.getAbsolutePath(), e);
            }
        }

        private boolean parseRangesAndUpdateHeaders() throws IOException {
            try {
                addHeaderIfNotExists(HttpHeaderNames.ACCEPT_RANGES, HttpHeaderValues.BYTES);
                contentStart = 0;
                expectedContentLength = raf.length();
                range = parseRange(raf.length());
                if (range == null) {
                    addHeaderIfNotExists(HttpHeaderNames.CONTENT_LENGTH, expectedContentLength);
                } else {
                    contentStart = range.getFirst();
                    expectedContentLength = range.getSecond() - range.getFirst() + 1;
                    setHeader(HttpHeaderNames.CONTENT_LENGTH, expectedContentLength);
                    setHeader(HttpHeaderNames.CONTENT_RANGE,
                              "bytes " + range.getFirst() + "-" + range.getSecond() + "/" + raf.length());
                }

                return true;
            } catch (IllegalArgumentException e) {
                Exceptions.ignore(e);
                return false;
            }
        }

        private void determineContentType() {
            contentType = MimeHelper.guessMimeType(name != null ? name : file.getName());
            addHeaderIfNotExists(HttpHeaderNames.CONTENT_TYPE, contentType);
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
                return ctx.writeAndFlush(Unpooled.EMPTY_BUFFER);
            } else if (isSSL()) {
                ctx.write(new ChunkedFile(raf, contentStart, expectedContentLength, BUFFER_SIZE));
                return ctx.writeAndFlush(LastHttpContent.EMPTY_LAST_CONTENT);
            } else {
                // Send file using zero copy approach!
                ctx.write(new DefaultFileRegion(raf.getChannel(), contentStart, expectedContentLength));
                return ctx.writeAndFlush(LastHttpContent.EMPTY_LAST_CONTENT);
            }
        }

        private Tuple<Long, Long> parseRange(long availableLength) {
            String header = wc.getHeader(HttpHeaderNames.RANGE);
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
    }

    /*
     * Determines if the current request should be compressed or not
     */
    private boolean canBeCompressed(String contentType) {
        String acceptEncoding = wc.getRequest().headers().get(HttpHeaderNames.ACCEPT_ENCODING);
        if (acceptEncoding == null || (!acceptEncoding.contains(HttpHeaderValues.GZIP) && !acceptEncoding.contains(
                HttpHeaderValues.DEFLATE))) {
            return false;
        }
        return MimeHelper.isCompressable(contentType);
    }

    private void installChunkedWriteHandler() {
        if (ctx.channel().pipeline().get(ChunkedWriteHandler.class) == null) {
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
        new SendFile().send(file);
    }

    /*
     * Signals an internal server error if one of the response method fails.
     */
    private void internalServerError(String debugMessage, Throwable t) {
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
        if (!ctx.channel().isOpen()) {
            ctx.channel().close();
        }
    }

    /*
     * Sets the Date and Cache headers for the HTTP Response
     */
    private void setDateAndCacheHeaders(long lastModifiedMillis, int cacheSeconds, boolean isPrivate) {
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
    private void setContentTypeHeader(String name) {
        addHeaderIfNotExists(HttpHeaderNames.CONTENT_TYPE, MimeHelper.guessMimeType(name));
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
                return;
            }
            if (HttpMethod.HEAD.equals(wc.getRequest().method())) {
                status(status);
                return;
            }

            String content =
                    Rythm.renderIfTemplateExists("view/errors/" + status.code() + FILETYPE_HTML, status, message);
            if (Strings.isEmpty(content)) {
                content = Rythm.renderIfTemplateExists("view/errors/error.html", status, message);
            }
            if (Strings.isEmpty(content)) {
                content = Rythm.renderIfTemplateExists("view/errors/default.html", status, message);
            }

            setHeader(HttpHeaderNames.CONTENT_TYPE, "text/html; charset=UTF-8");
            ByteBuf channelBuffer = wrapUTF8String(content);
            HttpResponse response = createFullResponse(status, false, channelBuffer);
            completeAndClose(commit(response));
        } catch (Exception e) {
            handleErrorInError(status, message, e);
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
                                                                + "Original Status Code: %s, Original Error: %s, URL: %s - %s (%s)",
                                                                status == null ? "null" : status.code(),
                                                                message,
                                                                wc == null || wc.getRequest() == null ?
                                                                "?" :
                                                                wc.getRequest().uri())
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
        template(HttpResponseStatus.OK, name, params);
    }

    /**
     * Renders the given Rythm template and sends the output as response.
     * <p>
     * By default caching will be disabled. If the file ends with .html, <tt>text/html; charset=UTF-8</tt> will be set
     * as content type. Otherwise the content type will be guessed from the filename.
     *
     * @param status the HTTP status to send. {@link HttpResponseStatus#OK} would be appropriate in most cases.
     * @param name   the name of the template to render. It's recommended to use files in /view/... and to place them
     *               in the resources directory.
     * @param params contains the parameters sent to the template
     * @see #template(String, Object...)
     */
    public void template(HttpResponseStatus status, String name, Object... params) {
        String content = null;
        wc.enableTiming(null);
        try {
            Object[] effectiveParams = fixParams(params);
            content = Rythm.render(name, effectiveParams);
        } catch (Exception e) {
            handleRythmError(name, e);
            return;
        }
        sendTemplateContent(status, name, content);
    }

    private void handleRythmError(String name, Exception e) {
        throw Exceptions.handle()
                        .to(RythmConfig.LOG)
                        .error(e)
                        .withSystemErrorMessage("Failed to render the template '%s': %s (%s)", name)
                        .handle();
    }

    protected void sendTemplateContent(HttpResponseStatus status, String name, String content) {
        try {
            if (name.endsWith("html")) {
                setHeader(HttpHeaderNames.CONTENT_TYPE, "text/html; charset=UTF-8");
            } else {
                setContentTypeHeader(name);
            }
            setDateAndCacheHeaders(System.currentTimeMillis(),
                                   cacheSeconds == null || Sirius.isDev() ? 0 : cacheSeconds,
                                   isPrivate);
            ByteBuf channelBuffer = wrapUTF8String(content);
            HttpResponse response = createFullResponse(status, true, channelBuffer);
            complete(commit(response));
        } catch (Exception e) {
            internalServerError("Cannot send content of template: " + name, e);
        }
    }

    /**
     * Tries to render the given Rythm template and sends the output as response.
     * <p>
     * If the template with the given name is not available, the fallback template is used.
     * <p>
     * By default caching will be disabled. If the file ends with .html, <tt>text/html; charset=UTF-8</tt> will be set
     * as content type. Otherwise the content type will be guessed from the filename.
     *
     * @param name         the name of the template to render. It's recommended to use files in /view/... and to place
     *                     them in the resources directory.
     * @param fallbackName the name which is used to search for the template, if no template with the given name does
     *                     exist.
     * @param params       contains the parameters sent to the template
     */
    public void alternativeTemplate(String name, String fallbackName, Object... params) {
        String content = null;
        wc.enableTiming(null);
        Object[] effectiveParams = fixParams(params);
        try {
            content = Rythm.renderIfTemplateExists(name, effectiveParams);
        } catch (Exception e) {
            handleRythmError(name, e);
        }
        if (Strings.isEmpty(content)) {
            try {
                content = Rythm.renderIfTemplateExists(fallbackName, effectiveParams);
            } catch (Exception e) {
                handleRythmError(fallbackName, e);
            }
        }

        sendTemplateContent(HttpResponseStatus.OK, name, content);
    }

    private Object[] fixParams(Object[] params) {
        if (params.length == 1 && params[0] instanceof Object[]) {
            return (Object[]) params[0];
        }
        return params;
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
            Object[] effectiveParams = fixParams(params);
            content = Rythm.renderIfTemplateExists(name + "_" + NLS.getCurrentLang() + FILETYPE_HTML, effectiveParams);
            if (Strings.isEmpty(content)) {
                content = Rythm.renderIfTemplateExists(name + "_" + NLS.getDefaultLanguage() + FILETYPE_HTML,
                                                       effectiveParams);
            }
            if (Strings.isEmpty(content)) {
                content = Rythm.render(name + FILETYPE_HTML, params);
            }
        } catch (Exception e) {
            handleRythmError(name, e);
        }
        sendTemplateContent(HttpResponseStatus.OK, name + FILETYPE_HTML, content);
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
            // Support caching...
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
            brb.execute(new TunnelHandler(url, failureHandler));
        } catch (Exception t) {
            internalServerError("Target-URL: " + url, t);
        }
    }

    private class TunnelHandler implements AsyncHandler<String> {

        private final String url;
        private Consumer<Integer> failureHandler;
        private final CallContext cc;
        private int responseCode = HttpResponseStatus.OK.code();
        private boolean contentLengthKnown;
        private volatile boolean failed;

        private TunnelHandler(String url, Consumer<Integer> failureHandler) {
            this.url = url;
            this.failureHandler = failureHandler;
            this.cc = CallContext.getCurrent();
        }

        @Override
        public STATE onStatusReceived(com.ning.http.client.HttpResponseStatus status) throws Exception {
            CallContext.setCurrent(cc);

            if (WebServer.LOG.isFINE()) {
                WebServer.LOG.FINE("Tunnel - STATUS %s for %s", status.getStatusCode(), wc.getRequestedURI());
            }
            if (status.getStatusCode() >= 200 && status.getStatusCode() < 300) {
                responseCode = status.getStatusCode();
                return STATE.CONTINUE;
            }
            if (status.getStatusCode() == HttpResponseStatus.NOT_MODIFIED.code()) {
                status(HttpResponseStatus.NOT_MODIFIED);
                return STATE.ABORT;
            }
            // Everything above 400 is an error and should be forwarded to the failure handler (if present)
            if (status.getStatusCode() >= 400 && failureHandler != null) {
                try {
                    failureHandler.accept(status.getStatusCode());
                    failed = true;
                } catch (Exception t) {
                    error(HttpResponseStatus.INTERNAL_SERVER_ERROR, Exceptions.handle(WebServer.LOG, t));
                }
            } else {
                // Even not technically an error, status codes 300..399 are handled here,
                // as the behaviour is the same as for a real error - which is also handled here, if no
                // failureHandler is present...
                error(HttpResponseStatus.valueOf(status.getStatusCode()));
            }
            return STATE.ABORT;
        }

        @Override
        public STATE onHeadersReceived(HttpResponseHeaders httpHeaders) throws Exception {
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

            long lastModified = forwardHeadersAndDetermineLastModified(httpHeaders);
            if (handleIfModifiedSince(lastModified)) {
                return STATE.ABORT;
            }

            if (!headers().contains(HttpHeaderNames.CONTENT_TYPE)) {
                setContentTypeHeader(name != null ? name : url);
            }
            setDateAndCacheHeaders(lastModified, cacheSeconds == null ? HTTP_CACHE : cacheSeconds, isPrivate);

            if (name != null) {
                setContentDisposition(name, download);
            }

            return STATE.CONTINUE;
        }

        private long forwardHeadersAndDetermineLastModified(HttpResponseHeaders httpHeaders) {
            FluentCaseInsensitiveStringsMap receivedHeaders = httpHeaders.getHeaders();

            long lastModified = 0;

            for (Map.Entry<String, List<String>> entry : receivedHeaders.entrySet()) {
                if ((Sirius.isDev() || !entry.getKey().startsWith("x-"))
                    && !NON_TUNNELLED_HEADERS.contains(entry.getKey())) {
                    if (HttpHeaderNames.LAST_MODIFIED.contentEqualsIgnoreCase(entry.getKey())) {
                        lastModified = parseLastModified(entry);
                    } else {
                        forwardHeaderValues(entry);
                    }
                    if (HttpHeaderNames.CONTENT_LENGTH.contentEqualsIgnoreCase(entry.getKey())) {
                        contentLengthKnown = true;
                    }
                }
            }

            return lastModified;
        }

        private void forwardHeaderValues(Map.Entry<String, List<String>> entry) {
            for (String value : entry.getValue()) {
                addHeaderIfNotExists(entry.getKey(), value);
            }
        }

        private long parseLastModified(Map.Entry<String, List<String>> entry) {
            try {
                return getHTTPDateFormat().parse(entry.getValue().get(0)).getTime();
            } catch (Exception e) {
                Exceptions.ignore(e);
                return 0;
            }
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
                        HttpUtil.setContentLength(response, bodyPart.getBodyByteBuffer().remaining());
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
            } catch (Exception e) {
                Exceptions.handle(e);
                return STATE.ABORT;
            }
        }

        @Override
        public String onCompleted() throws Exception {
            // If the request to tunnel failed and we successfully
            // invoked a failureHandler, we must not do any housekeeping
            // here but rely on the failure handler to take care of the request.
            if (failed) {
                return "";
            }

            CallContext.setCurrent(cc);

            if (WebServer.LOG.isFINE()) {
                WebServer.LOG.FINE("Tunnel - COMPLETE for %s", wc.getRequestedURI());
            }
            if (!wc.responseCommitted) {
                HttpResponse response =
                        createFullResponse(HttpResponseStatus.valueOf(responseCode), true, Unpooled.EMPTY_BUFFER);
                HttpUtil.setContentLength(response, 0);
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
                if (failureHandler != null) {
                    try {
                        failureHandler.accept(HttpResponseStatus.INTERNAL_SERVER_ERROR.code());
                        failed = true;
                        return;
                    } catch (Exception e) {
                        Exceptions.handle(WebServer.LOG, e);
                    }
                }
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
            throw Exceptions.createHandled()
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

        private void ensureCapacity(int length) throws IOException {
            if (buffer != null && buffer.writableBytes() < length) {
                flushBuffer(false);
            }
            if (buffer == null) {
                buffer = ctx.alloc().buffer(BUFFER_SIZE);
            }
        }

        private void flushBuffer(boolean last) throws IOException {
            if ((buffer == null || buffer.readableBytes() == 0) && !last) {
                if (buffer != null) {
                    buffer.release();
                    buffer = null;
                }
                return;
            }
            if (!ctx.channel().isOpen()) {
                open = false;
                if (buffer != null) {
                    buffer.release();
                    buffer = null;
                }
                throw new ClosedChannelException();
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
                addHeaderIfNotExists(HttpHeaderNames.CONTENT_TYPE, contentType);
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
                HttpUtil.setContentLength(response, initialBuffer.readableBytes());
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
