/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.http;

import java.util.Map;

/**
 * Permits to compute a http session secret per user or per session.
 * <p>
 * Instead of using the same security fingerprint secret for all sessions, this can be implemented to provide
 * a custom secret per user or session to further increase the security.
 */
public interface SessionSecretComputer {

    /**
     * Computes or provides a secret based on the decoded session.
     * <p>
     * Note that the secret itself must not be contained in the session data, as this would disable the session security
     * entirely. Rather the user ID or a similar session value can be used to determine a secret and safe session value.
     *
     * @param currentSession the decoded session data
     * @return the secret to use
     */
    String computeSecret(Map<String, String> currentSession);
}
