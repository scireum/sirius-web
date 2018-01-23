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

    public static final String CSRF_TOKEN = "CSRFToken";
    public static final String LAST_CSRF_RECOMPUTE = "lastCSRFRecompute";

    @ConfigValue("http.csrfTokenLifetime")
    private static Duration csrfTokenLifetime;

    /**
     * Returns the CSRF security-token of the current session. Internally recomputes the token if outdated.
     *
     * @return the CSRF security-token to protect sensitive links.
     */
    public String getCSRFToken(WebContext ctx) {
        Value lastCSRFRecompute = ctx.getSessionValue(LAST_CSRF_RECOMPUTE);

        if (isCSRFTokenOutdated(lastCSRFRecompute.asLong(-1L)) || lastCSRFRecompute.isEmptyString()) {
            ctx.setSessionValue(CSRF_TOKEN, UUID.randomUUID().toString());
            ctx.setSessionValue(LAST_CSRF_RECOMPUTE, Value.of(Instant.now().toEpochMilli()).asString());
        }

        return ctx.getSessionValue(CSRF_TOKEN).asString();
    }

    private boolean isCSRFTokenOutdated(long lastCSRFRecompute) {
        return Duration.between(Instant.ofEpochMilli(lastCSRFRecompute), Instant.now()).toMinutes()
               > csrfTokenLifetime.toMinutes();
    }
}
