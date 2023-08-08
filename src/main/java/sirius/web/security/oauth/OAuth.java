/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.security.oauth;

/**
 * Defines some constants used for OAuth2 authentication.
 */
public class OAuth {

    private OAuth() {
        // No instances allowed
    }

    /**
     * The token type bearer. It gets prepended in the authentication header and returned with the valid token.
     *
     * @see #OAUTH_TOKEN_TYPE
     */
    public static final String TOKEN_TYPE_BEARER = "bearer";

    /**
     * Determines the access token used to access the API.
     */
    public static final String OAUTH_ACCESS_TOKEN = "access_token";

    /**
     * Determines the client id that determines an OAuth client.
     */
    public static final String OAUTH_CLIENT_ID = "client_id";

    /**
     * Determines the shared client secret of an OAuth client.
     */
    public static final String OAUTH_CLIENT_SECRET = "client_secret";

    /**
     * Determines the authentication code used to obtain an access token or the response type containing a code.
     *
     * @see #OAUTH_RESPONSE_TYPE
     */
    public static final String OAUTH_CODE = "code";

    /**
     * Determines when an access token expires.
     */
    public static final String OAUTH_EXPIRES_IN = "expires_in";

    /**
     * Determines the grant type used to obtain an access token.
     */
    public static final String OAUTH_GRANT_TYPE = "grant_type";

    /**
     * Determines the password used for Resource Owner Password flow.
     */
    public static final String OAUTH_PASSWORD = "password";

    /**
     * Determines the redirect URI used to send an authentication code.
     */
    public static final String OAUTH_REDIRECT_URI = "redirect_uri";

    /**
     * Determines the refresh token used to obtain a new access token.
     */
    public static final String OAUTH_REFRESH_TOKEN = "refresh_token";

    /**
     * Determines the response type used to obtain an authentication code.
     *
     * @see #OAUTH_CODE
     */
    public static final String OAUTH_RESPONSE_TYPE = "response_type";

    /**
     * Determines the authorization scope of an access token or a request.
     */
    public static final String OAUTH_SCOPE = "scope";

    /**
     * Determines the state used to prevent CSRF attacks in authentication code flow.
     */
    public static final String OAUTH_STATE = "state";

    /**
     * Determines the token type used to access the API.
     *
     * @see #TOKEN_TYPE_BEARER
     */
    public static final String OAUTH_TOKEN_TYPE = "token_type";

    /**
     * Determines the username used for Resource Owner Password flow.
     */
    public static final String OAUTH_USERNAME = "username";

    /**
     * The grant type password. Used for Resource Owner Password flow.
     *
     * @see #OAUTH_GRANT_TYPE
     */
    public static final String GRANT_TYPE_PASSWORD = "password";

    /**
     * The grant type authorization code. Used for Authorization Code flow.
     *
     * @see #OAUTH_GRANT_TYPE
     */
    public static final String GRANT_TYPE_AUTH_CODE = "authorization_code";

    /**
     * The grant type refresh token. Used for Refresh Token flow.
     *
     * @see #OAUTH_GRANT_TYPE
     */
    public static final String GRANT_TYPE_REFRESH_TOKEN = "refresh_token";
}
