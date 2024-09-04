/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.security.oauth;

import com.auth0.jwt.JWT;
import com.auth0.jwt.exceptions.JWTDecodeException;
import com.fasterxml.jackson.databind.node.ObjectNode;

import java.time.LocalDateTime;
import java.time.ZoneOffset;

/**
 * Represents the tokens received from an OAuth authorization server. This is used to read the token in a standard
 * way, and then handle and store them in a custom way
 *
 * @param accessToken           the access token received from the authorization server
 * @param refreshToken          the refresh token received from the authorization server
 * @param type                  the type of the tokens received from the authorization server, e.g. "Bearer" or "MAC"
 * @param accessTokenExpiresAt  the date at which the access token expires, might be null if the server response
 *                              contains no information
 * @param refreshTokenExpiresAt the date at which the refresh token expires, might be estimated if no JWT bearer
 *                              token is given
 */
public record ReceivedTokens(String accessToken, String refreshToken, String type, LocalDateTime accessTokenExpiresAt,
                             LocalDateTime refreshTokenExpiresAt) {

    private static final long TWO_DAYS_IN_SECONDS = 2 * 24 * 60 * 60L;
    private static final int MINIMUM_REFRESH_EXPIRES_DAYS = 1;

    /**
     * Creates a new instance from a JSON response.
     *
     * @param response the JSON response received from the authorization server
     * @return the tokens received from the authorization server
     */
    public static ReceivedTokens fromJson(ObjectNode response) {
        String accessToken = response.required(OAuth.ACCESS_TOKEN).asText("");
        String refreshToken = response.required(OAuth.REFRESH_TOKEN).asText("");
        String type = response.required(OAuth.TOKEN_TYPE).asText("");
        long accessTokenExpiresIn = response.path(OAuth.EXPIRES_IN).asLong(0L);
        LocalDateTime accessTokenExpiresAt =
                accessTokenExpiresIn > 0 ? LocalDateTime.now().plusSeconds(accessTokenExpiresIn) : null;
        if (OAuth.TOKEN_TYPE_BEARER.equalsIgnoreCase(type)) {
            try {
                // Try to read the exact refresh token expiration date from the JWT token itself
                LocalDateTime refreshTokenExpiresAt =
                        JWT.decode(refreshToken).getExpiresAtAsInstant().atZone(ZoneOffset.UTC).toLocalDateTime();
                return new ReceivedTokens(accessToken, refreshToken, type, accessTokenExpiresAt, refreshTokenExpiresAt);
            } catch (JWTDecodeException exception) {
                // No valid JWT, fall back to implementation from OAuth expires_in or the default value
            }
        }

        // Check if the 'expires in' field, actually meant for the access token, is better than our refresh token
        // default expires value tomorrow.
        if (accessTokenExpiresIn > TWO_DAYS_IN_SECONDS) {
            return new ReceivedTokens(accessToken, refreshToken, type, accessTokenExpiresAt, accessTokenExpiresAt);
        }

        // Use default value tomorrow, we expect a refresh token to be valid at least for one more day
        LocalDateTime expiresDate = LocalDateTime.now().plusDays(MINIMUM_REFRESH_EXPIRES_DAYS);
        return new ReceivedTokens(accessToken, refreshToken, type, accessTokenExpiresAt, expiresDate);
    }
}
