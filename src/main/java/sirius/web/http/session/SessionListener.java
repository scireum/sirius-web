/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.http.session;

/**
 * Listeners can be registered to be notified when a session was created or invalidated.
 *
 * @author Andreas Haufler (aha@scireum.de)
 * @since 2014/05
 */
public interface SessionListener {

    /**
     * Notifies the listener, that a new session was created.
     *
     * @param session the newly created session
     */
    void sessionCreated(ServerSession session);

    /**
     * Notifies the listener, that a new session was invalidated.
     *
     * @param session the invalidated session
     */
    void sessionInvalidated(ServerSession session);
}
