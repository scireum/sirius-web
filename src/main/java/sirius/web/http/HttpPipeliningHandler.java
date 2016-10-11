/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.http;

import com.google.common.collect.Lists;
import io.netty.channel.ChannelDuplexHandler;
import io.netty.channel.ChannelHandlerContext;
import io.netty.channel.ChannelPromise;
import io.netty.handler.codec.http.FullHttpResponse;
import io.netty.handler.codec.http.HttpRequest;
import io.netty.handler.codec.http.LastHttpContent;
import io.netty.util.ReferenceCounted;

import java.util.List;

/**
 * Adds support for HTTP pipelining by ensuring that only requests are passed on, after a response for the last request
 * was sent.
 * <p>
 * Since pipelining is almost never used (with the expection of Safari on iOS in SOME cases), the implementation is
 * certainly not the best performing, but rather ensures simlicity and transparent request / response flows throughout
 * the rest of the pipeline.
 */
public class HttpPipeliningHandler extends ChannelDuplexHandler {

    private volatile HttpRequest currentRequest;
    private List<HttpRequest> bufferedRequests = Lists.newArrayList();

    @Override
    public void channelRead(ChannelHandlerContext ctx, Object msg) throws Exception {
        if (msg instanceof HttpRequest) {
            if (currentRequest == null) {
                currentRequest = (HttpRequest) msg;
                ctx.fireChannelRead(msg);
            } else {
                bufferedRequests.add((HttpRequest) msg);
            }
            return;
        }
        // As long as there is no conflicting request present,
        // we continue with the pipeline. This is especially required for web sockets,
        // which receive WebsocketFrames after the request and LastHttpContent were
        // received....
        if (currentRequest == null || (currentRequest != null && bufferedRequests.isEmpty())) {
            ctx.fireChannelRead(msg);
            return;
        }

        // If a conflicting request was put aside in the bufferedRequests list, we can safely
        // ignore the empty  LastHttpContent for it - we will emulate this in <tt>write</tt>
        if (msg instanceof LastHttpContent) {
            if (((LastHttpContent) msg).content().readableBytes() == 0) {
                ((LastHttpContent) msg).release();
                return;
            }
        }

        // If any other content is received (that would be another POST for example, we give up!) There
        // is no sane way to handle and support that correctly...
        if (msg instanceof ReferenceCounted) {
            ((ReferenceCounted) msg).release();
        }

        throw new IllegalStateException("HTTP Pipelining for requests with a body ist unsupported.");
    }

    @Override
    public void write(ChannelHandlerContext ctx, Object msg, ChannelPromise promise) throws Exception {
        super.write(ctx, msg, promise);
        if (msg instanceof FullHttpResponse || msg instanceof LastHttpContent) {
            if (currentRequest == null) {
                throw new IllegalStateException("Received a response without a request");
            }
            currentRequest = null;

            if (!bufferedRequests.isEmpty()) {
                currentRequest = bufferedRequests.remove(0);
                ctx.fireChannelRead(currentRequest);
                ctx.fireChannelRead(LastHttpContent.EMPTY_LAST_CONTENT);
            }
        }
    }
}
