/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.security.oauth;

import com.fasterxml.jackson.databind.node.ObjectNode;
import sirius.kernel.commons.URLBuilder;
import sirius.kernel.di.std.Register;
import sirius.web.services.JSONCall;

import java.io.IOException;
import java.net.URI;

/**
 * Provides utility methods to perform OAuth authentication flows.
 */
@Register(classes = OAuthAuthentication.class)
public class OAuthAuthentication {

    /**
     * Creates the URL to start the OAuth authentication flow. Will return a URL to which the user must be redirected
     * to start granting access to a resource.
     * <p>
     * This is to be used with confidential clients as no PKCE is used.
     *
     * @param oauthAuthUrl the url of the endpoint at the authorization server
     * @param clientId     the client id, which is registered at the authorization server
     * @param state        the state to be used to prevent CSRF attacks
     * @param redirectUrl  the url to redirect to after the authentication flow has been completed
     * @param scope        the scope to request
     * @return the url to start the OAuth authentication flow.
     */
    public String createOAuthAuthenticationFlowUrl(String oauthAuthUrl,
                                                   String clientId,
                                                   String state,
                                                   String redirectUrl,
                                                   String scope) {
        return new URLBuilder(oauthAuthUrl).addParameter(OAuth.RESPONSE_TYPE, OAuth.CODE)
                                           .addParameter(OAuth.CLIENT_ID, clientId)
                                           .addParameter(OAuth.GRANT_TYPE, OAuth.GRANT_TYPE_AUTH_CODE)
                                           .addParameter(OAuth.REDIRECT_URI, redirectUrl)
                                           .addParameter(OAuth.STATE, state)
                                           .addParameter(OAuth.SCOPE, scope)
                                           .build();
    }

    /**
     * After the authentication flow has been completed by returning an authentication code to the redirect URL, this
     * method can be used to obtain an access token in exchange for the authentication code.
     * <p>
     * This is to be used in the backend, where the client secret can be kept secret.
     *
     * @param authenticationCode the authentication code returned by the authorization server
     * @param oauthLoginUrl      the url of the login endpoint at the authorization server
     * @param clientId           the client id, which is registered at the authorization server
     * @param sharedSecret       the client secret, which is registered at the authorization server
     * @return the json response that should contain an access and refresh token in case of success
     * @throws IOException in case of a connection error
     */
    public ObjectNode performLoginByAuthCode(String authenticationCode,
                                             String oauthLoginUrl,
                                             String clientId,
                                             String sharedSecret) throws IOException {
        String loginUrl = new URLBuilder(oauthLoginUrl).addParameter(OAuth.CLIENT_SECRET, sharedSecret)
                                                       .addParameter(OAuth.CLIENT_ID, clientId)
                                                       .addParameter(OAuth.CODE, authenticationCode)
                                                       .addParameter(OAuth.GRANT_TYPE, OAuth.GRANT_TYPE_AUTH_CODE)
                                                       .build();
        return JSONCall.to(URI.create(loginUrl)).getInput();
    }
}
