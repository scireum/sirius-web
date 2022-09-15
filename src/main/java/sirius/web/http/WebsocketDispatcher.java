/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.http;

/**
 * Provides a way to support websockets for the built-in web server.
 * <p>
 * Websocket support is done by creating a class implementing this interface. By placing a {@link
 * sirius.kernel.di.std.Register} annotation, it is found by the {@link WebServerHandler} which then initiates the
 * websocket protocol for requests on the given URI.
 */
public interface WebsocketDispatcher {

    /**
     * Returns the URI which is used to determine if a request is eligble for upgrading to web sockets.
     *
     * @return the uri which is used to start websockets
     */
    String getWebsocketUri();

    /**
     * Creates a new session for a given request.
     *
     * @param ctx the request which was made before upgrading to websockets
     * @return a new session handling incoming and outgoing frames
     */
    WebsocketSession createSession(WebContext ctx);
}
