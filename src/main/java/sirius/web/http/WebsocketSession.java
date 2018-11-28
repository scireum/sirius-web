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
import io.netty.handler.codec.http.cookie.Cookie;
import io.netty.handler.codec.http.websocketx.TextWebSocketFrame;
import io.netty.handler.codec.http.websocketx.WebSocketFrame;
import sirius.kernel.async.CallContext;
import sirius.kernel.commons.Value;

import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * Created per active websocket by a {@link WebsocketDispatcher} to handle incoming and outgoing traffic.
 */
public abstract class WebsocketSession {

    private final Map<String, Value> queryString;
    private final Map<String, String> cookies;
    private final String lang;
    private final ChannelHandlerContext ctx;

    /**
     * Creates a new session for the given channel and request.
     *
     * @param ctx     the channel (context) which started the websocket
     * @param request the request made before upgrading to websocket
     */
    protected WebsocketSession(ChannelHandlerContext ctx, HttpRequest request) {
        WebContext webContext = CallContext.getCurrent().get(WebContext.class);
        webContext.setCtx(ctx);
        webContext.setRequest(request);

        this.ctx = ctx;
        queryString =
                webContext.getParameterNames().stream().collect(Collectors.toMap(Function.identity(), webContext::get));
        cookies = webContext.getCookies().stream().collect(Collectors.toMap(Cookie::name, Cookie::value));
        lang = webContext.getLang();
    }

    /**
     * Reads the given parameter from the query string of the underlying request.
     *
     * @param key the name of the parameter to fetch
     * @return the value contained in the parameter
     */
    protected Value get(String key) {
        return Value.of(queryString.get(key));
    }

    /**
     * Reads the given parameter from the query string of the underlying request.
     *
     * @param key the name of the parameter to fetch
     * @return the value contained in the parameter
     */
    protected Value getCookie(String key) {
        return Value.of(cookies.get(key));
    }

    /**
     * Reads the browser language from the "Accept-Language" header.
     *
     * @return the browser language.
     */
    public String getLang() {
        return lang;
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
