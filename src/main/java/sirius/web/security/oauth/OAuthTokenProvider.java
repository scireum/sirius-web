/*
 * Made with all the love in the world
 * by scireum in Stuttgart, Germany
 *
 * Copyright by scireum GmbH
 * https://www.scireum.de - info@scireum.de
 */

package sirius.web.security.oauth;

import sirius.kernel.di.std.AutoRegister;

import java.util.Optional;

/**
 * Provides implementation specific access to OAuth tokens.
 */
@AutoRegister
public interface OAuthTokenProvider {

    /**
     * Fetches the token identified by the given name, scoped to the current context but not tied to a specific user.
     *
     * @param tokenName the name of the token to fetch
     * @return an optional containing the token if available, or empty if none is found
     */
    Optional<String> fetchValidTokenForCurrentScope(String tokenName);
}
