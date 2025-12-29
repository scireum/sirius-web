/*
 * Made with all the love in the world
 * by scireum in Stuttgart, Germany
 *
 * Copyright by scireum GmbH
 * https://www.scireum.de - info@scireum.de
 */

package sirius.web.security.oauth;

import sirius.kernel.di.std.Parts;
import sirius.kernel.di.std.Register;

import java.util.Collection;
import java.util.Optional;

/**
 * Provides utility methods to work with OAuth tokens and services.
 */
@Register(classes = OAuthTokenProviderUtils.class)
public class OAuthTokenProviderUtils {

    @Parts(OAuthTokenProvider.class)
    private Collection<OAuthTokenProvider> providers;

    /**
     * Fetches a valid OAuth token for the current scope and the specified token name.
     *
     * @param tokenName the name of the token to fetch
     * @return an optional containing the valid access token if available, or empty if none is found
     */
    public Optional<String> fetchValidTokenForCurrentScope(String tokenName) {
        return providers.stream()
                        .map(provider -> provider.fetchValidTokenForCurrentScope(tokenName))
                        .flatMap(Optional::stream)
                        .findFirst();
    }
}
