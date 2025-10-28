/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.http;

import io.netty.handler.codec.http.websocketx.TextWebSocketFrame;
import io.netty.handler.codec.http.websocketx.WebSocketFrame;

/**
 * Created per active websocket by a {@link WebsocketDispatcher} to handle incoming and outgoing traffic.
 */
public abstract class WebsocketSession {

    private final WebContext webContext;

    /**
     * Creates a new session for the given channel and request.
     *
     * @param webContext the channel (context) which started the websocket
     */
    protected WebsocketSession(WebContext webContext) {
        this.webContext = webContext;
    }

    /**
     * Triggered when the websocket was sucessfully initialized.
     */
    public void onWebsocketOpened() {
    }

    /**
     * Tirggered when the websocket is closed.
     */
    public void onWebsocketClosed() {
    }

    /**
     * Invoked for each frame received via the websocket.
     *
     * @param frame the received frame
     */
    public abstract void onFrame(WebSocketFrame frame);

    /**
     * Sends a given string as text frame to the client.
     *
     * @param text the string to send
     */
    public void sendMessage(String text) {
        webContext.getChannelHandlerContext().writeAndFlush(new TextWebSocketFrame(text));
    }

    /**
     * Returns the original request of this websocket connection.
     *
     * @return the original request of this websocket connection
     */
    public WebContext getContext() {
        return webContext;
    }
}
