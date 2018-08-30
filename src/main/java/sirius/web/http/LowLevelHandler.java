/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.http;

import io.netty.buffer.ByteBuf;
import io.netty.channel.ChannelDuplexHandler;
import io.netty.channel.ChannelHandler;
import io.netty.channel.ChannelHandlerContext;
import io.netty.channel.ChannelPromise;

import java.net.InetSocketAddress;
import java.net.SocketAddress;

/**
 * Handler for low-level events in the HTTP pipeline.
 * <p>
 * Performs statistical tasks and performs basic filtering based on firewall rules.
 */
@ChannelHandler.Sharable
class LowLevelHandler extends ChannelDuplexHandler {
    static final LowLevelHandler INSTANCE = new LowLevelHandler();

    @Override
    public void connect(ChannelHandlerContext ctx,
                        SocketAddress remoteAddress,
                        SocketAddress localAddress,
                        ChannelPromise future) throws Exception {
        if (WebServer.connections.incrementAndGet() < 0) {
            WebServer.connections.set(0);
        }
        IPRange.RangeSet filter = WebServer.getIPFilter();
        if (!filter.isEmpty()) {
            if (!filter.accepts(((InetSocketAddress) remoteAddress).getAddress())) {
                if (WebServer.blocks.incrementAndGet() < 0) {
                    WebServer.blocks.set(0);
                }
                ctx.channel().close();
                future.setSuccess();
                return;
            }
        }
        super.connect(ctx, remoteAddress, localAddress, future);
    }

    @Override
    public void channelRead(ChannelHandlerContext ctx, Object msg) throws Exception {
        if (msg instanceof ByteBuf) {
            int messageSize = ((ByteBuf) msg).readableBytes();
            if (WebServer.bytesIn.addAndGet(messageSize) < 0) {
                WebServer.bytesIn.set(0);
            }
            if (WebServer.messagesIn.incrementAndGet() < 0) {
                WebServer.messagesIn.set(0);
            }
            ctx.pipeline().get(WebServerHandler.class).inbound(messageSize);
        }
        super.channelRead(ctx, msg);
    }

    @Override
    public void write(ChannelHandlerContext ctx, Object msg, ChannelPromise promise) throws Exception {
        if (msg instanceof ByteBuf) {
            int messageSize = ((ByteBuf) msg).readableBytes();
            if (WebServer.bytesOut.addAndGet(messageSize) < 0) {
                WebServer.bytesOut.set(0);
            }
            if (WebServer.messagesOut.incrementAndGet() < 0) {
                WebServer.messagesOut.set(0);
            }
            ctx.pipeline().get(WebServerHandler.class).outbound(messageSize);
        }
        super.write(ctx, msg, promise);
    }
}
