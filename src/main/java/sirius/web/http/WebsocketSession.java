/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.http;

import io.netty.channel.ChannelHandlerContext;
import io.netty.handler.codec.http.HttpHeaderNames;
import io.netty.handler.codec.http.HttpRequest;
import io.netty.handler.codec.http.QueryStringDecoder;
import io.netty.handler.codec.http.cookie.Cookie;
import io.netty.handler.codec.http.cookie.ServerCookieDecoder;
import io.netty.handler.codec.http.websocketx.TextWebSocketFrame;
import io.netty.handler.codec.http.websocketx.WebSocketFrame;
import sirius.kernel.async.CallContext;
import sirius.kernel.commons.Strings;
import sirius.kernel.commons.Value;
import sirius.kernel.commons.Values;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * Created per active websocket by a {@link WebsocketDispatcher} to handle incoming and outgoing traffic.
 */
public abstract class WebsocketSession {

    private final QueryStringDecoder queryString;
    private final Map<String, String> cookies;
    private ChannelHandlerContext channelCtx;
    private WebContext ctx;

    /**
     * Creates a new session for the given channel and request.
     *
     * @param ctx     the channel (context) which started the websocket
     * @param request the request made before upgrading to websocket
     */
    protected WebsocketSession(ChannelHandlerContext ctx, HttpRequest request) {
        this.channelCtx = ctx;
        this.ctx = CallContext.getCurrent().get(WebContext.class);
        this.ctx.setCtx(ctx);
        this.ctx.setRequest(request);
        this.queryString = new QueryStringDecoder(request.uri());

        String cookieHeader = request.headers().get(HttpHeaderNames.COOKIE);
        if (Strings.isFilled(cookieHeader)) {
            cookies = ServerCookieDecoder.LAX.decode(cookieHeader)
                                             .stream()
                                             .collect(Collectors.toMap(Cookie::name, Cookie::value));
        } else {
            cookies = Collections.emptyMap();
        }
    }

    public WebContext getWebContext() {
        return ctx;
    }

    /**
     * Reads the given parameter from the query string of the underlying request.
     *
     * @param key the name of the parameter to fetch
     * @return the value contained in the parameter
     */
    protected Value get(String key) {
        List<String> list = queryString.parameters().get(key);
        if (list == null) {
            return Value.EMPTY;
        }

        return Values.of(list).at(0);
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
        channelCtx.writeAndFlush(new TextWebSocketFrame(text));
    }
}
