/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.http.session;

import javax.annotation.Nullable;
import java.util.Optional;
import java.util.stream.Stream;

/**
 * Can be used to change the behaviour of the session storage.
 * <p>
 * By default al session data is stored as {@link sirius.web.http.session.MemoryServerSession} which is essentially
 * a Java <tt>Map</tt>. Registering a class implementing this interface, this can be changed i.e. to support
 * clustering.
 */
public interface SessionStorage {

    /**
     * Returns the session associated with the given id or an empty value if no such session exists.
     * <p>
     * Invoking this method will "touch" the session and mark it as recently accessed.
     *
     * @param id the id of the desired session
     * @return the session with the given id wrapped as {@link java.util.Optional}
     */
    Optional<ServerSession> getSession(String id);

    /**
     * Returns the session for the given ID or <tt>null</tt> if no such session exists.
     * <p>
     * In contrast to {@link #getSession(String)}, this is a technical API which will not be considered an "access"
     * for the session.
     *
     * @param id the id of the desired session
     * @return the session with the given ID or <tt>null</tt> if no such session exists
     */
    @Nullable
    ServerSession findSession(String id);

    /**
     * Creates a new session.
     *
     * @return a newly created session
     */
    ServerSession createSession();

    /**
     * Returns a stream of all known session ids.
     *
     * @return a stream of all session ids known to the system
     */
    Stream<String> getSessions();

    /**
     * Returns the number of active sessions
     *
     * @return the number of sessions
     */
    int getNumberOfSessions();
}
