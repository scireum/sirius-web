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
import io.netty.handler.codec.DecoderException;
import io.netty.handler.codec.http.HttpObjectAggregator;
import io.netty.handler.codec.http.HttpRequest;
import io.netty.handler.codec.http.websocketx.WebSocketFrame;
import io.netty.handler.codec.http.websocketx.WebSocketServerProtocolHandler;
import sirius.kernel.async.CallContext;
import sirius.kernel.async.Tasks;
import sirius.kernel.di.std.Part;
import sirius.kernel.health.Exceptions;
import sirius.kernel.nls.NLS;

import javax.net.ssl.SSLHandshakeException;
import java.io.IOException;
import java.nio.channels.ClosedChannelException;

/**
 * Inserted into the pipeline by the {@link WebServerInitializer} if an implementation of {@link WebsocketDispatcher}
 * is present.
 */
public class WebsocketHandler extends ChannelDuplexHandler {

    private WebsocketSession websocketSession;
    private WebsocketDispatcher websocketDispatcher;
    private CallContext currentCall;

    @Part
    private static Tasks tasks;

    public static final String EXECUTOR_WEBSOCKETS = "websockets";

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
        if (e instanceof SSLHandshakeException || e.getCause() instanceof SSLHandshakeException) {
            SSLWebServerInitializer.LOG.FINE(e);
        } else if (e instanceof ClosedChannelException || e instanceof IOException || e instanceof DecoderException) {
            WebServer.LOG.FINE("Received an error for a websocket: %s", NLS.toUserString(e));
        } else {
            Exceptions.handle()
                      .to(WebServer.LOG)
                      .error(e)
                      .withSystemErrorMessage("Received an error for a websocket - %s (%s)")
                      .handle();
        }
    }

    @Override
    public void channelUnregistered(ChannelHandlerContext ctx) throws Exception {
        if (websocketSession != null) {
            websocketSession.onWebsocketClosed();
            WebServer.websockets.decrementAndGet();
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
            currentCall = WebServerHandler.initializeContext(ctx, (HttpRequest) msg, false);
            websocketSession = websocketDispatcher.createSession(currentCall.get(WebContext.class));
            WebServer.websockets.incrementAndGet();
            setupWebsocketPipeline(ctx, msg);
            return;
        }

        if (msg instanceof WebSocketFrame) {
            handleFrame(ctx, (WebSocketFrame) msg);
            return;
        }

        super.channelRead(ctx, msg);
    }

    private void handleFrame(ChannelHandlerContext ctx, WebSocketFrame msg) {
        if (websocketSession != null) {
            dispatchFrame(ctx, msg);
        } else {
            terminateOnError(ctx, msg);
        }
    }

    private void terminateOnError(ChannelHandlerContext ctx, WebSocketFrame msg) {
        try {
            ctx.channel().close();
        } catch (Exception e) {
            Exceptions.handle(WebServer.LOG, e);
        } finally {
            msg.release();
        }
    }

    private void dispatchFrame(ChannelHandlerContext ctx, WebSocketFrame msg) {
        tasks.executor(EXECUTOR_WEBSOCKETS)
             .dropOnOverload(() -> terminateOnError(ctx, msg))
             .start(() -> handleFrameInOwnThread(msg));
    }

    private void handleFrameInOwnThread(WebSocketFrame msg) {
        try {
            CallContext.setCurrent(currentCall);
            websocketSession.onFrame(msg);
        } finally {
            msg.release();
        }
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
