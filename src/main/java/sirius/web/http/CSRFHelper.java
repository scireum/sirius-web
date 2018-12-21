/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.http;

import sirius.kernel.commons.Value;
import sirius.kernel.di.std.ConfigValue;
import sirius.kernel.di.std.Register;

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
     * @param ctx the request to read the token from
     * @return the CSRF security-token to protect sensitive links.
     */
    public String getCSRFToken(WebContext ctx) {
        Value lastCSRFRecompute = ctx.getSessionValue(LAST_CSRF_RECOMPUTE);

        if (isCSRFTokenOutdated(lastCSRFRecompute.asLong(-1L))) {
            recomputeCSRFToken(ctx);
        }

        return ctx.getSessionValue(CSRF_TOKEN).asString();
    }

    private boolean isCSRFTokenOutdated(long lastCSRFRecompute) {
        return Duration.between(Instant.ofEpochMilli(lastCSRFRecompute), Instant.now()).compareTo(csrfTokenLifetime)
               > 0;
    }

    public void recomputeCSRFToken(WebContext ctx) {
        ctx.setSessionValue(PREVIOUS_CSRF_TOKEN, ctx.getSessionValue(CSRF_TOKEN).asString());
        ctx.setSessionValue(CSRF_TOKEN, UUID.randomUUID().toString());
        ctx.setSessionValue(LAST_CSRF_RECOMPUTE, Value.of(Instant.now().toEpochMilli()).asString());
    }
}
