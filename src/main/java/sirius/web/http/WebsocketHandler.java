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

/**
 * Inserted into the pipeline by the {@link WebServerInitializer} if an implementation of {@link WebsocketDispatcher}
 * is present.
 */
public class WebsocketHandler extends ChannelDuplexHandler {

    private WebsocketSession websocketSession;
    private final WebsocketDispatcher websocketDispatcher;
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
    public void exceptionCaught(ChannelHandlerContext channelHandlerContext, Throwable throwable) throws Exception {
        if (throwable instanceof SSLHandshakeException || throwable.getCause() instanceof SSLHandshakeException) {
            SSLWebServerInitializer.LOG.FINE(throwable);
        } else if (throwable instanceof IOException || throwable instanceof DecoderException) {
            WebServer.LOG.FINE("Received an error for a websocket: %s", NLS.toUserString(throwable));
        } else {
            Exceptions.handle()
                      .to(WebServer.LOG)
                      .error(throwable)
                      .withSystemErrorMessage("Received an error for a websocket - %s (%s)")
                      .handle();
        }
    }

    @Override
    public void channelUnregistered(ChannelHandlerContext channelHandlerContext) throws Exception {
        if (websocketSession != null) {
            websocketSession.onWebsocketClosed();
            WebServer.websockets.decrementAndGet();
        }

        super.channelUnregistered(channelHandlerContext);
    }

    @Override
    public void userEventTriggered(ChannelHandlerContext channelHandlerContext, Object event) throws Exception {
        if (websocketSession != null && event instanceof WebSocketServerProtocolHandler.HandshakeComplete) {
            websocketSession.onWebsocketOpened();
        }

        super.userEventTriggered(channelHandlerContext, event);
    }

    @Override
    public void channelRead(ChannelHandlerContext channelHandlerContext, Object message) throws Exception {
        if ((message instanceof HttpRequest httpRequest) && isWebsocketRequest(httpRequest)) {
            currentCall = WebServerHandler.initializeContext(channelHandlerContext, httpRequest, false);
            websocketSession = websocketDispatcher.createSession(currentCall.getOrCreateSubContext(WebContext.class));
            WebServer.websockets.incrementAndGet();
            setupWebsocketPipeline(channelHandlerContext, message);
            return;
        }

        if (message instanceof WebSocketFrame webSocketFrame) {
            handleFrame(channelHandlerContext, webSocketFrame);
            return;
        }

        super.channelRead(channelHandlerContext, message);
    }

    private void handleFrame(ChannelHandlerContext channelHandlerContext, WebSocketFrame frame) {
        if (websocketSession != null) {
            dispatchFrame(channelHandlerContext, frame);
        } else {
            terminateOnError(channelHandlerContext, frame);
        }
    }

    private void terminateOnError(ChannelHandlerContext channelHandlerContext, WebSocketFrame frame) {
        try {
            channelHandlerContext.channel().close();
        } catch (Exception exception) {
            Exceptions.handle(WebServer.LOG, exception);
        } finally {
            frame.release();
        }
    }

    private void dispatchFrame(ChannelHandlerContext channelHandlerContext, WebSocketFrame frame) {
        tasks.executor(EXECUTOR_WEBSOCKETS)
             .dropOnOverload(() -> terminateOnError(channelHandlerContext, frame))
             .start(() -> handleFrameInOwnThread(frame));
    }

    private void handleFrameInOwnThread(WebSocketFrame frame) {
        try {
            CallContext.setCurrent(currentCall);
            websocketSession.onFrame(frame);
        } finally {
            frame.release();
        }
    }

    private void setupWebsocketPipeline(ChannelHandlerContext channelHandlerContext, Object message) throws Exception {
        HttpObjectAggregator handler = new HttpObjectAggregator(8192);
        channelHandlerContext.pipeline().addBefore("websockethandler", "aggregator", handler);
        channelHandlerContext.pipeline()
                             .addAfter("aggregator",
                                       "websocketx",
                                       new WebSocketServerProtocolHandler(((HttpRequest) message).uri(), "xmpp", true));
        channelHandlerContext.pipeline().remove("idler");
        channelHandlerContext.pipeline().remove("compressor");
        handler.channelRead(channelHandlerContext, message);
    }

    private boolean isWebsocketRequest(HttpRequest request) {
        if (websocketDispatcher == null || request.uri() == null) {
            return false;
        }

        return request.uri().startsWith(websocketDispatcher.getWebsocketUri());
    }
}
