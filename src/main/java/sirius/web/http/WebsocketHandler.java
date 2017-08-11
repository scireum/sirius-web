/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.http;

import io.netty.channel.ChannelDuplexHandler;
import io.netty.channel.ChannelHandlerContext;
import io.netty.handler.codec.http.HttpObjectAggregator;
import io.netty.handler.codec.http.HttpRequest;
import io.netty.handler.codec.http.websocketx.WebSocketFrame;
import io.netty.handler.codec.http.websocketx.WebSocketServerProtocolHandler;
import sirius.kernel.health.Exceptions;

import javax.net.ssl.SSLHandshakeException;
import java.io.IOException;
import java.nio.channels.ClosedChannelException;

/**
 * Inserted into the pipeline by the {@link WebServerInitializer} if an implementation of {@link WebsocketDispatcher}
 * is
 * present.
 */
public class WebsocketHandler extends ChannelDuplexHandler {

    private WebsocketSession websocketSession;
    private WebsocketDispatcher websocketDispatcher;

    /**
     * Creates a new handler (one per connection) talking to the given dispatcher.
     *
     * @param websocketDispatcher the dispatcher responsible for creating a {@link WebsocketSession}
     */
    public WebsocketHandler(WebsocketDispatcher websocketDispatcher) {
        this.websocketDispatcher = websocketDispatcher;
    }

    @Override
    public void exceptionCaught(ChannelHandlerContext ctx, Throwable e) throws Exception {
        if (e instanceof SSLHandshakeException) {
            SSLWebServerInitializer.LOG.FINE(e);
        } else if (e instanceof ClosedChannelException || e instanceof IOException) {
            WebServer.LOG.FINE(e);
        } else {
            Exceptions.handle(WebServer.LOG, e);
        }
    }

    @Override
    public void channelUnregistered(ChannelHandlerContext ctx) throws Exception {
        if (websocketSession != null) {
            websocketSession.onWebsocketClosed();
            WebServer.websockets--;
        }

        super.channelUnregistered(ctx);
    }

    @Override
    public void userEventTriggered(ChannelHandlerContext ctx, Object evt) throws Exception {
        if (websocketSession != null && evt instanceof WebSocketServerProtocolHandler.HandshakeComplete) {
            websocketSession.onWebsocketOpened();
        }

        super.userEventTriggered(ctx, evt);
    }

    @Override
    public void channelRead(ChannelHandlerContext ctx, Object msg) throws Exception {
        if (msg instanceof HttpRequest && isWebsocketRequest((HttpRequest) msg)) {
            websocketSession = websocketDispatcher.createSession(ctx, (HttpRequest) msg);
            WebServer.websockets++;
            setupWebsocketPipeline(ctx, msg);
            return;
        }

        if (msg instanceof WebSocketFrame) {
            if (websocketSession != null) {
                websocketSession.onFrame((WebSocketFrame) msg);
            } else {
                try {
                    ctx.channel().close();
                } catch (Exception e) {
                    Exceptions.handle(WebServer.LOG, e);
                }
            }
            ((WebSocketFrame) msg).release();

            return;
        }

        super.channelRead(ctx, msg);
    }

    private void setupWebsocketPipeline(ChannelHandlerContext ctx, Object msg) throws Exception {
        HttpObjectAggregator handler = new HttpObjectAggregator(8192);
        ctx.pipeline().addBefore("websockethandler", "aggregator", handler);
        ctx.pipeline()
           .addAfter("aggregator",
                     "websocketx",
                     new WebSocketServerProtocolHandler(((HttpRequest) msg).uri(), "xmpp", true));
        ctx.pipeline().remove("idler");
        ctx.pipeline().remove("compressor");
        handler.channelRead(ctx, msg);
    }

    private boolean isWebsocketRequest(HttpRequest req) {
        if (websocketDispatcher == null || req.uri() == null) {
            return false;
        }

        return req.uri().startsWith(websocketDispatcher.getWebsocketUri());
    }
}
