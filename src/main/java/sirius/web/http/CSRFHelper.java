/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.http;

import sirius.kernel.async.CallContext;
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
        return getCSRFToken(CallContext.getCurrent().getOrCreateSubContext(WebContext.class));
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

    private boolean isCSRFTokenOutdated(long lastCSRFRecompute) {
        return Duration.between(Instant.ofEpochMilli(lastCSRFRecompute), Instant.now()).compareTo(csrfTokenLifetime)
               > 0;
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
}
