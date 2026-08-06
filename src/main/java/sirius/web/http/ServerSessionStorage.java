/*
 * Made with all the love in the world
 * by scireum in Stuttgart, Germany
 *
 * Copyright by scireum GmbH
 * https://www.scireum.de - info@scireum.de
 */

package sirius.web.http;

import java.util.Map;

/**
 * Permits an application to store the client session of selected requests on the server instead of transporting
 * it via the session cookie.
 * <p>
 * If an implementation is registered and claims responsibility for a request (via
 * {@link #isResponsibleFor(WebContext)}), the {@link WebContext} will neither read nor write the session cookie
 * (nor the session pinning cookie) for that request. Instead, the session values are loaded once (lazily, on the
 * first session access) and persisted back at response time if they were modified. If no implementation is
 * registered, or it does not claim responsibility, the built-in cookie based client session remains fully in
 * charge.
 * <p>
 * This is commonly used to provide a server side session for requests which are authenticated via a bearer token
 * (which can carry a session id) instead of a session cookie. Implementations are registered via
 * {@link sirius.kernel.di.std.Register} against this interface.
 */
public interface ServerSessionStorage {

    /**
     * Determines if this storage manages the session of the given request.
     * <p>
     * This is invoked at most once per request, lazily, when the session is first accessed. Implementations should
     * be cheap for non-matching requests (e.g. check for the presence of a header first) and may cache expensive
     * intermediate results (e.g. a validated token) as request attributes.
     *
     * @param webContext the request to check
     * @return <tt>true</tt> if the session of this request is managed by this storage, <tt>false</tt> otherwise
     */
    boolean isResponsibleFor(WebContext webContext);

    /**
     * Loads the stored session values for the given request.
     * <p>
     * Returning an empty map starts a fresh session. If an exception is thrown, the framework falls back to an
     * empty session and suppresses persistence for this request, so that a transient error cannot overwrite the
     * stored session with an empty one.
     *
     * @param webContext the request to load the session for
     * @return the previously stored session values or an empty map if there is no stored session yet
     */
    Map<String, String> loadSession(WebContext webContext);

    /**
     * Persists the given session values for the given request.
     * <p>
     * This is only invoked at response time, if the session was modified and the response is not cacheable. An
     * empty map signals a cleared session (e.g. a logout) and should delete the stored session.
     *
     * @param webContext the request to persist the session for
     * @param session    the session values to persist
     */
    void persistSession(WebContext webContext, Map<String, String> session);
}
