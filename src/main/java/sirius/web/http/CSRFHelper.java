/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.http;

import sirius.kernel.commons.Strings;
import sirius.kernel.commons.Value;
import sirius.kernel.di.std.ConfigValue;
import sirius.kernel.di.std.Register;
import sirius.pasta.noodle.sandbox.NoodleSandbox;

import java.time.Duration;
import java.time.Instant;
import java.util.UUID;

/**
 * A helper class which manages CSRF security-tokens within the user session.
 */
@Register(classes = CSRFHelper.class)
public class CSRFHelper {

    /**
     * Contains the parameter name of the CSRF token.
     */
    public static final String CSRF_TOKEN = "CSRFToken";

    /**
     * Contains the parameter name of the previous CSRF token.
     */
    public static final String PREVIOUS_CSRF_TOKEN = "previousCSRFToken";

    /**
     * Contains the parameter name of the date at which point the csrf token was last recomputed.
     */
    public static final String LAST_CSRF_RECOMPUTE = "lastCSRFRecompute";

    @ConfigValue("http.csrfTokenLifetime")
    private static Duration csrfTokenLifetime;

    /**
     * Returns the CSRF security-token of the current session. Internally recomputes the token if outdated.
     *
     * @return the CSRF security-token to protect sensitive links.
     */
    @NoodleSandbox(NoodleSandbox.Accessibility.GRANTED)
    public String getCSRFToken() {
        return getCSRFToken(WebContext.getCurrent());
    }

    /**
     * Returns the CSRF security-token of the current session. Internally recomputes the token if outdated.
     *
     * @param webContext the request to read the token from
     * @return the CSRF security-token to protect sensitive links.
     */
    public String getCSRFToken(WebContext webContext) {
        Value lastCSRFRecompute = webContext.getSessionValue(LAST_CSRF_RECOMPUTE);

        if (isCSRFTokenOutdated(lastCSRFRecompute.asLong(-1L))) {
            recomputeCSRFToken(webContext);
        }

        return webContext.getSessionValue(CSRF_TOKEN).asString();
    }

    /**
     * Forces an explicit re-computation of the CSRF token.
     *
     * @param webContext the web context to recompute the token for
     */
    public void recomputeCSRFToken(WebContext webContext) {
        webContext.setSessionValue(PREVIOUS_CSRF_TOKEN, webContext.getSessionValue(CSRF_TOKEN).asString());
        webContext.setSessionValue(CSRF_TOKEN, UUID.randomUUID().toString());
        webContext.setSessionValue(LAST_CSRF_RECOMPUTE, Value.of(Instant.now().toEpochMilli()).asString());
    }

    /**
     * Determines if the given request contains a valid CSRF token.
     *
     * @param webContext the request to check
     * @return <tt>true</tt> if the request contains a valid CSRF token, <tt>false</tt> otherwise
     */
    public boolean hasValidCsrfToken(WebContext webContext) {
        String requestToken = webContext.get(CSRF_TOKEN).asString();
        String sessionToken = webContext.getSessionValue(CSRF_TOKEN).asString();
        String lastSessionToken = webContext.getSessionValue(PREVIOUS_CSRF_TOKEN).asString();

        return Strings.isFilled(requestToken) && isValidRequestToken(requestToken, sessionToken, lastSessionToken);
    }

    private boolean isCSRFTokenOutdated(long lastCSRFRecompute) {
        Duration timeSinceLastRecompute = Duration.between(Instant.ofEpochMilli(lastCSRFRecompute), Instant.now());
        return timeSinceLastRecompute.compareTo(csrfTokenLifetime) > 0;
    }

    private boolean isValidRequestToken(String requestToken, String sessionToken, String lastSessionToken) {
        return Strings.areEqual(requestToken, sessionToken) || Strings.areEqual(requestToken, lastSessionToken);
    }
}
