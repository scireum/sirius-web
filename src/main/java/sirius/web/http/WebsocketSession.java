/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.http;

import io.netty.channel.ChannelHandlerContext;
import io.netty.handler.codec.http.HttpRequest;
import io.netty.handler.codec.http.QueryStringDecoder;
import io.netty.handler.codec.http.websocketx.TextWebSocketFrame;
import io.netty.handler.codec.http.websocketx.WebSocketFrame;
import sirius.kernel.commons.Value;
import sirius.kernel.commons.Values;

/**
 * Created per active websocket by a {@link WebsocketDispatcher} to handle incoming and outgoing traffic.
 */
public abstract class WebsocketSession {

    private final QueryStringDecoder queryString;
    private ChannelHandlerContext ctx;
    private final HttpRequest request;

    /**
     * Creates a new session for the given channel and request.
     *
     * @param ctx     the channel (context) which started the websocket
     * @param request the request made before upgrading to websocket
     */
    public WebsocketSession(ChannelHandlerContext ctx, HttpRequest request) {
        this.ctx = ctx;
        this.request = request;
        this.queryString = new QueryStringDecoder(request.getUri());
    }

    /**
     * Reads the given parameter from the query string of the underlying request.
     *
     * @param key the name of the parameter to fetch
     * @return the value contained in the parameter
     */
    protected Value get(String key) {
        return Values.of(queryString.parameters().get(key)).at(0);
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
     * Sents a given string as text frame to the client.
     *
     * @param text the string to send
     */
    public void sendMessage(String text) {
        ctx.writeAndFlush(new TextWebSocketFrame(text));
    }
}
