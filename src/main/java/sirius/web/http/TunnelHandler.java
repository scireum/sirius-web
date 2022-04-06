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
import io.netty.channel.Channel;
import io.netty.channel.ChannelFuture;
import io.netty.handler.codec.http.DefaultHttpContent;
import io.netty.handler.codec.http.DefaultLastHttpContent;
import io.netty.handler.codec.http.HttpHeaderNames;
import io.netty.handler.codec.http.HttpHeaders;
import io.netty.handler.codec.http.HttpResponse;
import io.netty.handler.codec.http.HttpResponseStatus;
import io.netty.handler.codec.http.HttpUtil;
import io.netty.handler.codec.http.LastHttpContent;
import org.asynchttpclient.AsyncHandler;
import org.asynchttpclient.HttpResponseBodyPart;
import org.asynchttpclient.netty.request.NettyRequest;
import sirius.kernel.Sirius;
import sirius.kernel.async.CallContext;
import sirius.kernel.commons.Processor;
import sirius.kernel.commons.Strings;
import sirius.kernel.commons.ValueHolder;
import sirius.kernel.commons.Watch;
import sirius.kernel.health.Exceptions;
import sirius.kernel.health.HandledException;
import sirius.kernel.nls.NLS;

import javax.net.ssl.SSLSession;
import java.net.InetSocketAddress;
import java.nio.channels.ClosedChannelException;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.function.Consumer;
import java.util.function.IntConsumer;
import java.util.stream.Collectors;

/**
 * Performs tunneling into a request by reading from another.
 */
class TunnelHandler implements AsyncHandler<String> {

    private static final Set<String> NON_TUNNELLED_HEADERS =
            Set.of(HttpHeaderNames.TRANSFER_ENCODING.toString().toLowerCase(),
                   HttpHeaderNames.SERVER.toString().toLowerCase(),
                   HttpHeaderNames.CONTENT_ENCODING.toString().toLowerCase(),
                   HttpHeaderNames.EXPIRES.toString().toLowerCase(),
                   HttpHeaderNames.CACHE_CONTROL.toString().toLowerCase());

    private static final Set<Integer> PASS_THROUGH_STATUS = Set.of(HttpResponseStatus.FOUND.code(),
                                                                   HttpResponseStatus.NOT_MODIFIED.code(),
                                                                   HttpResponseStatus.MOVED_PERMANENTLY.code(),
                                                                   HttpResponseStatus.TEMPORARY_REDIRECT.code());

    private static final Set<String> ALLOW_MULTIPLE_HEADERS =
            Set.of(HttpHeaderNames.SET_COOKIE.toString().toLowerCase());

    private final Response response;
    private final WebContext webContext;
    private final String url;
    private final Processor<ByteBuf, Optional<ByteBuf>> transformer;
    private final IntConsumer failureHandler;
    private final Consumer<TunnelHandler> completionHandler;
    private final CallContext cc;
    private final Watch watch;
    private volatile long timeToDns = -1;
    private volatile long timeToConnectAttempt = -1;
    private volatile long timeToConnect = -1;
    private volatile long timeToHandshake = -1;
    private volatile long timeToRequestSent = -1;

    private int responseCode = HttpResponseStatus.OK.code();
    private boolean contentLengthKnown;

    private volatile boolean failed;

    TunnelHandler(Response response,
                  String url,
                  Processor<ByteBuf, Optional<ByteBuf>> transformer,
                  IntConsumer failureHandler,
                  Consumer<TunnelHandler> completionHandler) {
        this.response = response;
        this.webContext = response.wc;
        this.url = url;
        this.transformer = transformer;
        this.failureHandler = failureHandler;
        this.completionHandler = completionHandler;
        this.cc = CallContext.getCurrent();
        this.watch = Watch.start();
    }

    @Override
    public void onHostnameResolutionSuccess(String name, List<InetSocketAddress> addresses) {
        this.timeToDns = watch.elapsedMillis();
    }

    @Override
    public void onTcpConnectAttempt(InetSocketAddress remoteAddress) {
        this.timeToConnectAttempt = watch.elapsedMillis();
    }

    @Override
    public void onTcpConnectSuccess(InetSocketAddress remoteAddress, Channel connection) {
        this.timeToConnect = watch.elapsedMillis();
    }

    @Override
    public void onTlsHandshakeSuccess(SSLSession sslSession) {
        this.timeToHandshake = watch.elapsedMillis();
    }

    @Override
    public void onRequestSend(NettyRequest request) {
        this.timeToRequestSent = watch.elapsedMillis();
    }

    @Override
    public State onStatusReceived(org.asynchttpclient.HttpResponseStatus status) throws Exception {
        logTiming(status);

        CallContext.setCurrent(cc);

        if (WebServer.LOG.isFINE()) {
            WebServer.LOG.FINE("Tunnel - STATUS %s for %s", status.getStatusCode(), response.wc.getRequestedURI());
        }
        if ((status.getStatusCode() >= 200 && status.getStatusCode() < 300)
            || PASS_THROUGH_STATUS.contains(status.getStatusCode())) {
            // We either had a successful response or a state which we want to forward (NOT_MODIFIED, redirects ...)
            responseCode = status.getStatusCode();
            return State.CONTINUE;
        }

        // From this point on, the request will be handled here and we don't want the onComplete handler to
        // do any housekeeping...
        failed = true;

        // Everything above 400 is an error and should be forwarded to the failure handler (if present)
        if (status.getStatusCode() >= 400 && failureHandler != null) {
            try {
                failureHandler.accept(status.getStatusCode());
            } catch (Exception t) {
                response.error(HttpResponseStatus.INTERNAL_SERVER_ERROR, Exceptions.handle(WebServer.LOG, t));
            }
        } else {
            // Even not technically an error, status codes 300..399 are handled here,
            // as the behaviour is the same as for a real error - we only emit the error code without revealing any
            // internals..
            response.error(HttpResponseStatus.valueOf(status.getStatusCode()));
        }

        return State.ABORT;
    }

    @Override
    public State onHeadersReceived(HttpHeaders httpHeaders) throws Exception {
        CallContext.setCurrent(cc);

        if (webContext.responseCommitted) {
            if (WebServer.LOG.isFINE()) {
                WebServer.LOG.FINE("Tunnel - BLOCKED HEADERS (already sent) for %s", webContext.getRequestedURI());
            }
            return State.CONTINUE;
        }
        if (WebServer.LOG.isFINE()) {
            WebServer.LOG.FINE("Tunnel - HEADERS for %s", webContext.getRequestedURI());
        }

        long lastModified = forwardHeadersAndDetermineLastModified(httpHeaders);
        if (response.handleIfModifiedSince(lastModified)) {
            return State.ABORT;
        }

        overrideContentTypeIfNecessary();

        response.setDateAndCacheHeaders(lastModified,
                                        response.cacheSeconds == null ? Response.HTTP_CACHE : response.cacheSeconds,
                                        response.isPrivate);

        if (response.name != null) {
            response.setContentDisposition(response.name, response.download);
        }

        return State.CONTINUE;
    }

    /**
     * Overrides the {@link HttpHeaderNames#CONTENT_TYPE} header if the current value is not clearly specified.
     */
    private void overrideContentTypeIfNecessary() {
        String currentType = response.headers().get(HttpHeaderNames.CONTENT_TYPE);

        if (Strings.isEmpty(currentType) || MimeHelper.APPLICATION_OCTET_STREAM.equals(currentType)) {
            response.setHeader(HttpHeaderNames.CONTENT_TYPE,
                               MimeHelper.guessMimeType(response.name != null ? response.name : url));
        }
    }

    private long forwardHeadersAndDetermineLastModified(HttpHeaders httpHeaders) {
        ValueHolder<Long> lastModified = ValueHolder.of(0L);

        httpHeaders.entries().stream().filter(this::shouldForwardHeader).forEach(entry -> {
            if (HttpHeaderNames.LAST_MODIFIED.contentEqualsIgnoreCase(entry.getKey())) {
                lastModified.accept(parseLastModified(entry));
            } else if (HttpHeaderNames.CONTENT_LENGTH.contentEqualsIgnoreCase(entry.getKey())) {
                if (transformer == null) {
                    // If a transformer is present, we cannot trust or forward the content length as it might
                    // change (e.g. the transformer being a GZIP inflater)...
                    contentLengthKnown = true;
                    forwardHeaderValues(entry);
                }
            } else {
                forwardHeaderValues(entry);
            }
        });

        return lastModified.get();
    }

    private boolean shouldForwardHeader(Map.Entry<String, String> entry) {
        if (entry.getKey().startsWith("x-") || entry.getKey().startsWith("X-")) {
            // Non-standard headers (e.g. generated by Amazon S3) are only forwarded in development system.
            return Sirius.isDev();
        }

        return !NON_TUNNELLED_HEADERS.contains(entry.getKey().toLowerCase());
    }

    private void forwardHeaderValues(Map.Entry<String, String> entry) {
        if (ALLOW_MULTIPLE_HEADERS.contains(entry.getKey().toLowerCase())) {
            response.addHeader(entry.getKey(), entry.getValue());
        } else {
            response.addHeaderIfNotExists(entry.getKey(), entry.getValue());
        }
    }

    private long parseLastModified(Map.Entry<String, String> entry) {
        try {
            return response.getHTTPDateFormat().parse(entry.getValue()).getTime();
        } catch (Exception e) {
            Exceptions.ignore(e);
            return 0;
        }
    }

    @Override
    public State onBodyPartReceived(HttpResponseBodyPart bodyPart) throws Exception {
        try {
            CallContext.setCurrent(cc);

            if (WebServer.LOG.isFINE()) {
                WebServer.LOG.FINE("Tunnel - CHUNK: %s for %s (Last: %s)",
                                   bodyPart,
                                   webContext.getRequestedURI(),
                                   bodyPart.isLast());
            }
            if (!response.ctx.channel().isOpen()) {
                return State.ABORT;
            }

            if (!webContext.responseCommitted) {
                if (bodyPart.isLast() && transformer == null) {
                    ByteBuf data = Unpooled.wrappedBuffer(bodyPart.getBodyByteBuffer());
                    commitAndCompleteResponse(bodyPart, data);
                    return State.CONTINUE;
                }
                commitResponse();
            }

            if (bodyPart.isLast()) {
                completeResponse(bodyPart);
            } else if (transformer != null) {
                ByteBuf data = Unpooled.wrappedBuffer(bodyPart.getBodyByteBuffer());
                transformer.apply(data)
                           .map(DefaultHttpContent::new)
                           .ifPresent(message -> response.contentionAwareWrite(message, false));
                data.release();
            } else {
                ByteBuf data = Unpooled.wrappedBuffer(bodyPart.getBodyByteBuffer());
                Object msg = response.responseChunked ? new DefaultHttpContent(data) : data;
                response.contentionAwareWrite(msg, false);
            }
            return State.CONTINUE;
        } catch (HandledException e) {
            Exceptions.ignore(e);
            return State.ABORT;
        } catch (Exception e) {
            Exceptions.handle(e);
            return State.ABORT;
        }
    }

    private void commitAndCompleteResponse(HttpResponseBodyPart bodyPart, ByteBuf data) {
        HttpResponse res = response.createFullResponse(HttpResponseStatus.valueOf(responseCode), true, data);
        HttpUtil.setContentLength(res, bodyPart.getBodyByteBuffer().remaining());
        response.complete(response.commit(res));
    }

    private void completeResponse(HttpResponseBodyPart bodyPart) throws Exception {
        if (transformer != null) {
            transformLastContentAndComplete(bodyPart);
        } else if (response.responseChunked) {
            ByteBuf data = Unpooled.wrappedBuffer(bodyPart.getBodyByteBuffer());
            ChannelFuture writeFuture = response.ctx.writeAndFlush(new DefaultLastHttpContent(data));
            response.complete(writeFuture);
        } else {
            ByteBuf data = Unpooled.wrappedBuffer(bodyPart.getBodyByteBuffer());
            response.ctx.channel().write(data);
            ChannelFuture writeFuture = response.ctx.writeAndFlush(LastHttpContent.EMPTY_LAST_CONTENT);
            response.complete(writeFuture);
        }
    }

    private void transformLastContentAndComplete(HttpResponseBodyPart bodyPart) throws Exception {
        // If all previous buffers were non-empty - we have to process this buffer and finally an empty
        // one to signal the transformer that processing has completed. If this buffer is already empty,
        // we can skip this step...
        ByteBuf lastBufferToProcess = Unpooled.wrappedBuffer(bodyPart.getBodyByteBuffer());
        boolean lastBufferWasNonEmpty = lastBufferToProcess.isReadable();
        ByteBuf lastResult = transformer.apply(lastBufferToProcess).orElse(null);
        lastBufferToProcess.release();
        ByteBuf completeResult = lastBufferWasNonEmpty ? transformer.apply(Unpooled.EMPTY_BUFFER).orElse(null) : null;

        // Based on the processing above we might either end up with two buffers or with a single one.
        // Now we figure out which is simply sent and which is sent while obtaining the completion future
        // to finish the response...
        if (lastResult != null && completeResult != null) {
            response.contentionAwareWrite(new DefaultHttpContent(lastResult), false);
            ChannelFuture writeFuture = response.ctx.writeAndFlush(new DefaultLastHttpContent(completeResult));
            response.complete(writeFuture);
        } else if (completeResult != null) {
            throw new IllegalStateException("lastResult was null but completeResult wasn't!");
        } else if (lastResult != null) {
            ChannelFuture writeFuture = response.ctx.writeAndFlush(new DefaultLastHttpContent(lastResult));
            response.complete(writeFuture);
        } else {
            ChannelFuture writeFuture = response.ctx.writeAndFlush(LastHttpContent.EMPTY_LAST_CONTENT);
            response.complete(writeFuture);
        }
    }

    private void commitResponse() {
        if (contentLengthKnown) {
            response.commit(response.createResponse(HttpResponseStatus.valueOf(responseCode), true));
        } else {
            response.commit(response.createChunkedResponse(HttpResponseStatus.valueOf(responseCode), true));
        }
    }

    @Override
    public String onCompleted() throws Exception {
        if (completionHandler != null) {
            completionHandler.accept(this);
        }

        // If the request to tunnel failed, and we successfully
        // invoked a failureHandler, we must not do any housekeeping
        // here but rely on the failure handler to take care of the request.
        if (failed) {
            return "";
        }

        CallContext.setCurrent(cc);

        if (WebServer.LOG.isFINE()) {
            WebServer.LOG.FINE("Tunnel - COMPLETE for %s", webContext.getRequestedURI());
        }
        if (!webContext.responseCommitted) {
            HttpResponse res =
                    response.createFullResponse(HttpResponseStatus.valueOf(responseCode), true, Unpooled.EMPTY_BUFFER);
            HttpUtil.setContentLength(res, 0);
            response.complete(response.commit(res));
        } else if (!webContext.responseCompleted) {
            ChannelFuture writeFuture = response.ctx.writeAndFlush(LastHttpContent.EMPTY_LAST_CONTENT);
            response.complete(writeFuture);
        }
        return "";
    }

    private void logTiming(org.asynchttpclient.HttpResponseStatus status) {
        try {
            if (webContext.isLongCall() || webContext.scheduled == 0) {
                // No response time measurement for long running or aborted requests...
                return;
            }

            long ttfbMillis = watch.elapsedMillis();
            if (ttfbMillis > WebServer.getMaxTimeToFirstByte() && WebServer.getMaxTimeToFirstByte() > 0) {
                WebServer.LOG.WARN("Long running tunneling: %s (TTFB: %s)"
                                   + "%nDNS: %s, TCP-ATTEMPT: %s, TCP-CONNECT: %s, HANDSHAKE: %s, REQUEST-SENT: %s"
                                   + "%nURL:%s"
                                   + "%nTunnel URL:%s"
                                   + "%nStatus: %s"
                                   + "%nParameters:"
                                   + "%n%s"
                                   + "%nMDC:"
                                   + "%n%s%n",
                                   webContext.getRequestedURI(),
                                   NLS.convertDuration(ttfbMillis, true, true),
                                   safeConvertDuration(timeToDns),
                                   safeConvertDuration(timeToConnectAttempt),
                                   safeConvertDuration(timeToConnect),
                                   safeConvertDuration(timeToHandshake),
                                   safeConvertDuration(timeToRequestSent),
                                   webContext.getRequestedURL(),
                                   Strings.split(url, "?").getFirst(),
                                   status.getStatusCode(),
                                   webContext.getParameterNames()
                                             .stream()
                                             .map(param -> param + ": " + Strings.limit(webContext.get(param)
                                                                                                  .asString(), 50))
                                             .collect(Collectors.joining("\n")),
                                   cc);
            }
        } catch (Exception e) {
            Exceptions.handle(e);
        }
    }

    private String safeConvertDuration(long durationMillis) {
        if (durationMillis <= 0) {
            return "-";
        } else {
            return NLS.convertDuration(durationMillis, true, true);
        }
    }

    @Override
    public void onThrowable(Throwable t) {
        CallContext.setCurrent(cc);

        WebServer.LOG.WARN("Tunnel - ERROR %s for tunneling to '%s' performed at '%s'",
                           t.getMessage() + " (" + t.getClass().getName() + ")",
                           url,
                           webContext.getRequestedURI());
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
            response.error(HttpResponseStatus.INTERNAL_SERVER_ERROR, Exceptions.handle(WebServer.LOG, t));
        }
    }
}
