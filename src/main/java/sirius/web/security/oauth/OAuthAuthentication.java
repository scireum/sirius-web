/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.security.oauth;

import sirius.kernel.commons.Context;
import sirius.kernel.commons.URLBuilder;
import sirius.kernel.di.std.Register;
import sirius.web.services.JSONCall;

import java.io.IOException;
import java.net.URI;
import java.nio.charset.StandardCharsets;

/**
 * Provides utility methods to perform OAuth authentication flows.
 */
@Register(classes = OAuthAuthentication.class)
public class OAuthAuthentication {

    /**
     * Creates a URL that requests authorization by user consent for the specified OAuth scopes using the authorization
     * code flow.
     * <p>
     * The URL can be used to redirect the user to the OAuth provider's authorization endpoint, where they can grant
     * access to the requested scopes.
     * <p>
     * The URL asks for offline access to ensure that the refresh token is long-lived.
     *
     * @param endpoint    the URL of the OAuth authorization endpoint
     * @param clientId    the client ID registered with the OAuth provider
     * @param redirectUrl the URL to which the user will be redirected after authorization
     * @param scope       the scope of access requested by the application
     * @param state       a unique state parameter to prevent CSRF attacks, may also contain additional data such as an
     *                    entity ID or other context information that will be needed after redirection back
     * @return the complete authorization URL to redirect the user to
     */
    public String createAuthorizeConsentUrl(String endpoint,
                                            String clientId,
                                            String redirectUrl,
                                            String scope,
                                            String state) {
        return new URLBuilder(endpoint).addParameter(OAuth.RESPONSE_TYPE, OAuth.CODE)
                                       .addParameter(OAuth.CLIENT_ID, clientId)
                                       .addParameter(OAuth.REDIRECT_URI, redirectUrl)
                                       .addParameter(OAuth.SCOPE, scope)
                                       .addParameter(OAuth.STATE, state)
                                       .addParameter(OAuth.PROMPT, OAuth.PROMPT_CONSENT)
                                       .addParameter(OAuth.ACCESS_TYPE, OAuth.ACCESS_TYPE_OFFLINE)
                                       .build();
    }

    /**
     * Requests access and refresh tokens from the OAuth authorization server using the provided authorization code.
     *
     * @param endpoint          the URL of the OAuth token endpoint
     * @param clientId          the client ID registered with the OAuth provider
     * @param clientSecret      the client secret registered with the OAuth provider
     * @param authorizationCode the authorization code received from the OAuth provider after user consent
     * @param redirectUri       the redirect URI used during the authorization request, if applicable
     * @return the received tokens containing access and refresh tokens, as well as their types and expiration dates
     * @throws IOException if an error occurs while making the HTTP request to the OAuth server
     */
    public ReceivedTokens requestAccessTokenViaAuthorizationCode(String endpoint,
                                                                 String clientId,
                                                                 String clientSecret,
                                                                 String authorizationCode,
                                                                 String redirectUri) throws IOException {
        JSONCall call = JSONCall.to(URI.create(endpoint));

        call.getOutcall()
            .postData(Context.create()
                             .set(OAuth.GRANT_TYPE, OAuth.GRANT_TYPE_AUTH_CODE)
                             .set(OAuth.CODE, authorizationCode)
                             .set(OAuth.REDIRECT_URI, redirectUri)
                             .set(OAuth.CLIENT_ID, clientId)
                             .set(OAuth.CLIENT_SECRET, clientSecret), StandardCharsets.UTF_8);

        return ReceivedTokens.fromJson(call.getInput());
    }

    /**
     * Requests a new access token using the refresh token provided by the OAuth authorization server.
     * <p>
     * The returned tokens may include a new refresh token, depending on the OAuth provider's implementation.
     *
     * @param endpoint     the URL of the OAuth token endpoint
     * @param clientId     the client ID registered with the OAuth provider
     * @param clientSecret the client secret registered with the OAuth provider
     * @param refreshToken the refresh token received from the OAuth provider, used to obtain a new access token
     * @return the received tokens containing the new access token, its type, and expiration date
     * @throws IOException if an error occurs while making the HTTP request to the OAuth server
     */
    public ReceivedTokens requestAccessTokenViaRefreshToken(String endpoint,
                                                            String clientId,
                                                            String clientSecret,
                                                            String refreshToken) throws IOException {
        JSONCall call = JSONCall.to(URI.create(endpoint));

        call.getOutcall()
            .postData(Context.create()
                             .set(OAuth.GRANT_TYPE, OAuth.GRANT_TYPE_REFRESH_TOKEN)
                             .set(OAuth.REFRESH_TOKEN, refreshToken)
                             .set(OAuth.CLIENT_ID, clientId)
                             .set(OAuth.CLIENT_SECRET, clientSecret), StandardCharsets.UTF_8);

        return ReceivedTokens.fromJson(call.getInput());
    }
}
