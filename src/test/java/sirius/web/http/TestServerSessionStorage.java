/*
 * Made with all the love in the world
 * by scireum in Stuttgart, Germany
 *
 * Copyright by scireum GmbH
 * https://www.scireum.de - info@scireum.de
 */

package sirius.web.http;

import sirius.kernel.commons.Strings;
import sirius.kernel.di.std.Register;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * Provides a server session storage for tests which is only responsible for requests carrying the
 * {@link #SESSION_HEADER} - all other requests (i.e. the whole existing test suite) remain on the built-in
 * cookie based client session.
 * <p>
 * Like {@link TestFirewall} this is gated behind a framework flag which is disabled in the packaged test-jar
 * (see <tt>component-test-web.conf</tt>) and only enabled by the local <tt>test.conf</tt> - other libraries using
 * this test-jar must not have this storage registered, as it would compete with their own implementation for the
 * single {@link ServerSessionStorage} part slot.
 */
@Register(framework = "web.test-server-session-storage")
public class TestServerSessionStorage implements ServerSessionStorage {

    /**
     * Requests carrying this header are handled by this storage. The header value is used as session id.
     */
    public static final String SESSION_HEADER = "X-Test-Server-Session";

    /**
     * Using this session id makes {@link #loadSession(WebContext)} fail to test the fail-open behavior.
     */
    public static final String FAILING_SESSION_ID = "fail-load";

    private static final Map<String, Map<String, String>> sessions = new ConcurrentHashMap<>();
    private static final AtomicInteger persistCalls = new AtomicInteger();
    private static final AtomicInteger responsibilityChecks = new AtomicInteger();

    @Override
    public boolean isResponsibleFor(WebContext webContext) {
        responsibilityChecks.incrementAndGet();
        return Strings.isFilled(webContext.getHeader(SESSION_HEADER));
    }

    @Override
    public Map<String, String> loadSession(WebContext webContext) {
        String sessionId = webContext.getHeader(SESSION_HEADER);
        if (FAILING_SESSION_ID.equals(sessionId)) {
            throw new IllegalStateException("Simulated session load failure");
        }

        return sessions.getOrDefault(sessionId, Collections.emptyMap());
    }

    @Override
    public void persistSession(WebContext webContext, Map<String, String> session) {
        persistCalls.incrementAndGet();
        String sessionId = webContext.getHeader(SESSION_HEADER);
        if (session.isEmpty()) {
            sessions.remove(sessionId);
        } else {
            sessions.put(sessionId, new HashMap<>(session));
        }
    }

    /**
     * Returns the stored session values for the given session id.
     *
     * @param sessionId the session id to look up
     * @return the stored values or an empty map if no session is stored
     */
    public static Map<String, String> getStoredSession(String sessionId) {
        return sessions.getOrDefault(sessionId, Collections.emptyMap());
    }

    /**
     * Determines if a session is stored for the given session id.
     *
     * @param sessionId the session id to look up
     * @return <tt>true</tt> if a session is stored, <tt>false</tt> otherwise
     */
    public static boolean hasStoredSession(String sessionId) {
        return sessions.containsKey(sessionId);
    }

    /**
     * Counts the invocations of {@link #persistSession(WebContext, Map)} so far.
     *
     * @return the number of persist calls
     */
    public static int getPersistCalls() {
        return persistCalls.get();
    }

    /**
     * Counts the invocations of {@link #isResponsibleFor(WebContext)} so far, i.e. how often the session was
     * initialized.
     *
     * @return the number of responsibility checks
     */
    public static int getResponsibilityChecks() {
        return responsibilityChecks.get();
    }
}
