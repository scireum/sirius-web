/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.security.oauth;

import com.fasterxml.jackson.databind.node.ObjectNode;
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
     * @return the tokens in case of success
     * @throws IOException in case of a connection error
     */
    public ReceivedTokens performLoginByAuthCode(String authenticationCode,
                                                 String oauthLoginUrl,
                                                 String clientId,
                                                 String sharedSecret) throws IOException {
        return performLoginByAuthCode(authenticationCode, oauthLoginUrl, clientId, sharedSecret, null);
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
     * @param redirectUri        the redirect URI used to send the authentication code back to the client
     * @return the tokens in case of success
     * @throws IOException in case of a connection error
     */
    public ReceivedTokens performLoginByAuthCode(String authenticationCode,
                                                 String oauthLoginUrl,
                                                 String clientId,
                                                 String sharedSecret,
                                                 String redirectUri) throws IOException {
        Context postData = Context.create();
        postData.set(OAuth.CLIENT_SECRET, sharedSecret);
        postData.set(OAuth.CLIENT_ID, clientId);
        postData.set(OAuth.CODE, authenticationCode);
        postData.set(OAuth.GRANT_TYPE, OAuth.GRANT_TYPE_AUTH_CODE);
        if (redirectUri != null) {
            postData.set(OAuth.REDIRECT_URI, redirectUri);
        }

        JSONCall call = JSONCall.to(URI.create(oauthLoginUrl));
        call.getOutcall().postData(postData, StandardCharsets.UTF_8);
        ObjectNode response = call.getInput();

        return ReceivedTokens.fromJson(response);
    }

    /**
     * After the authentication flow has been completed by returning an authentication code to the redirect URL, this
     * method can be used to obtain an access token in exchange for the authentication code.
     * <p>
     * This is to be used in the backend, where the client secret can be kept secret.
     *
     * @param refreshToken  the authentication code returned by the authorization server
     * @param oauthLoginUrl the url of the login endpoint at the authorization server
     * @param clientId      the client id, which is registered at the authorization server
     * @param sharedSecret  the client secret, which is registered at the authorization server
     * @return the tokens in case of success
     * @throws IOException in case of a connection error
     */
    public ReceivedTokens performRefreshToken(String refreshToken,
                                              String oauthLoginUrl,
                                              String clientId,
                                              String sharedSecret) throws IOException {
        Context postData = Context.create();
        postData.set(OAuth.CLIENT_SECRET, sharedSecret);
        postData.set(OAuth.CLIENT_ID, clientId);
        postData.set(OAuth.REFRESH_TOKEN, refreshToken);
        postData.set(OAuth.GRANT_TYPE, OAuth.GRANT_TYPE_REFRESH_TOKEN);

        JSONCall call = JSONCall.to(URI.create(oauthLoginUrl));
        call.getOutcall().postData(postData, StandardCharsets.UTF_8);
        ObjectNode response = call.getInput();

        return ReceivedTokens.fromJson(response);
    }
}
