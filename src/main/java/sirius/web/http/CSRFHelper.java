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
     * Contains the parameter name of the date at which point the csrf token was last recomputed.
     */
    public static final String LAST_CSRF_RECOMPUTE = "lastCSRFRecompute";

    @ConfigValue("http.csrfTokenLifetime")
    private static Duration csrfTokenLifetime;

    @ConfigValue("http.csrfTokenAutoRenew")
    private static Duration csrfTokenAutoRenew;

    /**
     * Returns the CSRF security-token of the current session. Internally recomputes the token if outdated.
     *
     * @param ctx the request to read the token from
     * @return the CSRF security-token to protect sensitive links.
     */
    public String getCSRFToken(WebContext ctx) {
        return getCSRFToken(ctx, true);
    }

    /**
     * Returns the CSRF security-token of the current session. Internally recomputes the token if outdated.
     * <p>
     * In checks, the parameter autoRenew should be set to false as autoRenew uses {@link CSRFHelper#csrfTokenAutoRenew} which
     * might be earlier than the {@link CSRFHelper#csrfTokenLifetime real lifetime} of the token. If autoRenew
     * is set to true the tokens are renewed after the set time to avoid the token expiring while
     * staying on one site. Therefore the token is renewed in advance
     *
     * @param ctx       the request to read the token from
     * @param autoRenew whether the token should be recomputed in advance
     * @return the CSRF security-token to protect sensitive links.
     */
    public String getCSRFToken(WebContext ctx, boolean autoRenew) {
        Value lastCSRFRecompute = ctx.getSessionValue(LAST_CSRF_RECOMPUTE);

        if (isCSRFTokenOutdated(lastCSRFRecompute.asLong(-1L), autoRenew)) {
            ctx.setSessionValue(CSRF_TOKEN, UUID.randomUUID().toString());
            ctx.setSessionValue(LAST_CSRF_RECOMPUTE, Value.of(Instant.now().toEpochMilli()).asString());
        }

        return ctx.getSessionValue(CSRF_TOKEN).asString();
    }

    private boolean isCSRFTokenOutdated(long lastCSRFRecompute, boolean autoRenew) {
        return Duration.between(Instant.ofEpochMilli(lastCSRFRecompute), Instant.now()).toMinutes() > getTokenLifetime(
                autoRenew);
    }

    private long getTokenLifetime(boolean autoRenew) {
        if (autoRenew) {
            return csrfTokenAutoRenew.toMinutes();
        }
        return csrfTokenLifetime.toMinutes();
    }
}
