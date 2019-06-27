/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.http;

import com.google.common.collect.Sets;
import com.ning.http.client.AsyncHandler;
import com.ning.http.client.FluentCaseInsensitiveStringsMap;
import com.ning.http.client.HttpResponseBodyPart;
import com.ning.http.client.HttpResponseHeaders;
import io.netty.buffer.ByteBuf;
import io.netty.buffer.Unpooled;
import io.netty.channel.ChannelFuture;
import io.netty.handler.codec.http.DefaultHttpContent;
import io.netty.handler.codec.http.DefaultLastHttpContent;
import io.netty.handler.codec.http.HttpHeaderNames;
import io.netty.handler.codec.http.HttpResponse;
import io.netty.handler.codec.http.HttpResponseStatus;
import io.netty.handler.codec.http.HttpUtil;
import io.netty.handler.codec.http.LastHttpContent;
import sirius.kernel.Sirius;
import sirius.kernel.async.CallContext;
import sirius.kernel.commons.Strings;
import sirius.kernel.commons.Watch;
import sirius.kernel.health.Exceptions;
import sirius.kernel.health.HandledException;
import sirius.kernel.nls.NLS;

import java.nio.channels.ClosedChannelException;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Consumer;
import java.util.stream.Collectors;

/**
 * Performs tunneling into a request by reading from another.
 */
class TunnelHandler implements AsyncHandler<String> {

    private static final Set<String> NON_TUNNELLED_HEADERS =
            Sets.newHashSet(HttpHeaderNames.TRANSFER_ENCODING.toString(),
                            HttpHeaderNames.SERVER.toString(),
                            HttpHeaderNames.CONTENT_ENCODING.toString(),
                            HttpHeaderNames.EXPIRES.toString(),
                            HttpHeaderNames.CACHE_CONTROL.toString());

    private Response response;
    private WebContext webContext;
    private final String url;
    private Consumer<Integer> failureHandler;
    private final CallContext cc;
    private int responseCode = HttpResponseStatus.OK.code();
    private boolean contentLengthKnown;
    private volatile boolean failed;
    private Watch watch;

    TunnelHandler(Response response, String url, Consumer<Integer> failureHandler) {
        this.response = response;
        this.webContext = response.wc;
        this.url = url;
        this.failureHandler = failureHandler;
        this.cc = CallContext.getCurrent();
        this.watch = Watch.start();
    }

    @Override
    public STATE onStatusReceived(com.ning.http.client.HttpResponseStatus status) throws Exception {
        logTiming();

        CallContext.setCurrent(cc);

        if (WebServer.LOG.isFINE()) {
            WebServer.LOG.FINE("Tunnel - STATUS %s for %s", status.getStatusCode(), response.wc.getRequestedURI());
        }
        if (status.getStatusCode() >= 200 && status.getStatusCode() < 300) {
            responseCode = status.getStatusCode();
            return STATE.CONTINUE;
        }
        if (status.getStatusCode() == HttpResponseStatus.NOT_MODIFIED.code()) {
            response.status(HttpResponseStatus.NOT_MODIFIED);
            return STATE.ABORT;
        }
        // Everything above 400 is an error and should be forwarded to the failure handler (if present)
        if (status.getStatusCode() >= 400 && failureHandler != null) {
            try {
                failureHandler.accept(status.getStatusCode());
                failed = true;
            } catch (Exception t) {
                response.error(HttpResponseStatus.INTERNAL_SERVER_ERROR, Exceptions.handle(WebServer.LOG, t));
            }
        } else {
            // Even not technically an error, status codes 300..399 are handled here,
            // as the behaviour is the same as for a real error - which is also handled here, if no
            // failureHandler is present...
            response.error(HttpResponseStatus.valueOf(status.getStatusCode()));
        }
        return STATE.ABORT;
    }

    @Override
    public STATE onHeadersReceived(HttpResponseHeaders httpHeaders) throws Exception {
        CallContext.setCurrent(cc);

        if (webContext.responseCommitted) {
            if (WebServer.LOG.isFINE()) {
                WebServer.LOG.FINE("Tunnel - BLOCKED HEADERS (already sent) for %s", webContext.getRequestedURI());
            }
            return STATE.CONTINUE;
        }
        if (WebServer.LOG.isFINE()) {
            WebServer.LOG.FINE("Tunnel - HEADERS for %s", webContext.getRequestedURI());
        }

        long lastModified = forwardHeadersAndDetermineLastModified(httpHeaders);
        if (response.handleIfModifiedSince(lastModified)) {
            return STATE.ABORT;
        }

        if (!response.headers().contains(HttpHeaderNames.CONTENT_TYPE)) {
            response.setContentTypeHeader(response.name != null ? response.name : url);
        }
        response.setDateAndCacheHeaders(lastModified,
                                        response.cacheSeconds == null ? Response.HTTP_CACHE : response.cacheSeconds,
                                        response.isPrivate);

        if (response.name != null) {
            response.setContentDisposition(response.name, response.download);
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
            response.addHeaderIfNotExists(entry.getKey(), value);
        }
    }

    private long parseLastModified(Map.Entry<String, List<String>> entry) {
        try {
            return response.getHTTPDateFormat().parse(entry.getValue().get(0)).getTime();
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
                                   webContext.getRequestedURI(),
                                   bodyPart.isLast());
            }
            if (!response.ctx.channel().isOpen()) {
                return STATE.ABORT;
            }

            ByteBuf data = Unpooled.wrappedBuffer(bodyPart.getBodyByteBuffer());

            if (!webContext.responseCommitted) {
                if (bodyPart.isLast()) {
                    commitAndCompleteResponse(bodyPart, data);
                    return STATE.CONTINUE;
                }
                commitResponse();
            }

            if (bodyPart.isLast()) {
                completeResponse(data);
            } else {
                Object msg = response.responseChunked ? new DefaultHttpContent(data) : data;
                response.contentionAwareWrite(msg);
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

    private void commitAndCompleteResponse(HttpResponseBodyPart bodyPart, ByteBuf data) {
        HttpResponse res = response.createFullResponse(HttpResponseStatus.valueOf(responseCode), true, data);
        HttpUtil.setContentLength(res, bodyPart.getBodyByteBuffer().remaining());
        response.complete(response.commit(res));
    }

    private void completeResponse(ByteBuf data) {
        if (response.responseChunked) {
            ChannelFuture writeFuture = response.ctx.writeAndFlush(new DefaultLastHttpContent(data));
            response.complete(writeFuture);
        } else {
            response.ctx.channel().write(data);
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
        // If the request to tunnel failed and we successfully
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

    private void logTiming() {
        try {
            if (webContext.isLongCall() || webContext.scheduled == 0) {
                // No response time measurement for long running or aborted requests...
                return;
            }
            long ttfbMillis = watch.elapsedMillis();

            if (ttfbMillis > WebServer.getMaxTimeToFirstByte() && WebServer.getMaxTimeToFirstByte() > 0) {
                WebServer.LOG.WARN("Long running tunneling: %s (TTFB: %s)"
                                   + "%nURL:%s"
                                   + "%nTunnel URL:%s"
                                   + "%nParameters:"
                                   + "%n%s"
                                   + "%nMDC:"
                                   + "%n%s%n",
                                   webContext.getRequestedURI(),
                                   NLS.convertDuration(ttfbMillis, true, true),
                                   webContext.getRequestedURL(),
                                   Strings.split(url, "?").getFirst(),
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

    @Override
    public void onThrowable(Throwable t) {
        CallContext.setCurrent(cc);

        WebServer.LOG.WARN("Tunnel - ERROR %s for %s",
                           t.getMessage() + " (" + t.getMessage() + ")",
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
