/*
 * Made with all the love in the world
 * by scireum in Stuttgart, Germany
 *
 * Copyright by scireum GmbH
 * https://www.scireum.de - info@scireum.de
 */

package sirius.web.security.oauth;

import com.auth0.jwt.JWT;
import com.auth0.jwt.exceptions.JWTDecodeException;
import tools.jackson.databind.node.ObjectNode;
import sirius.kernel.commons.Strings;

import java.time.Instant;
import java.time.temporal.ChronoUnit;

/**
 * Represents the tokens received from an OAuth authorization server. This is used to read the token in a standard
 * way, and then handle and store them in a custom way
 *
 * @param accessToken           the access token received from the authorization server
 * @param refreshToken          the refresh token received from the authorization server, might be null if the server
 *                              response contains no refresh token (e.g. when refreshing an access token)
 * @param type                  the type of the tokens received from the authorization server, e.g. "Bearer" or "MAC"
 * @param accessTokenExpiresAt  the instant at which the access token expires, might be null if the server response
 *                              contains no information
 * @param refreshTokenExpiresAt the instant at which the refresh token expires, might be null if no refresh token was
 *                              returned or estimated if no JWT bearer token is given
 * @apiNote The expiries are instants, not local dates. Rendering one for a user or storing it in a date column
 * requires an explicit time zone, which is a decision only the caller can make.
 */
public record ReceivedTokens(String accessToken, String refreshToken, String type, Instant accessTokenExpiresAt,
                             Instant refreshTokenExpiresAt) {

    private static final long TWO_DAYS_IN_SECONDS = 2 * 24 * 60 * 60L;
    private static final int MINIMUM_REFRESH_EXPIRES_DAYS = 1;

    /**
     * Creates a new instance from a JSON response.
     *
     * @param response the JSON response received from the authorization server
     * @return the tokens received from the authorization server
     */
    public static ReceivedTokens fromJson(ObjectNode response) {
        String accessToken = response.required(OAuth.ACCESS_TOKEN).asString("");
        String refreshToken = response.path(OAuth.REFRESH_TOKEN).asString("");
        String type = response.required(OAuth.TOKEN_TYPE).asString("");
        long accessTokenExpiresIn = response.path(OAuth.EXPIRES_IN).asLong(0L);
        Instant accessTokenExpiresAt =
                accessTokenExpiresIn > 0 ? Instant.now().plusSeconds(accessTokenExpiresIn) : null;

        if (Strings.isEmpty(refreshToken)) {
            return new ReceivedTokens(accessToken, null, type, accessTokenExpiresAt, null);
        }

        if (OAuth.TOKEN_TYPE_BEARER.equalsIgnoreCase(type)) {
            try {
                // Try to read the exact refresh token expiration date from the JWT token itself
                Instant expiry = JWT.decode(refreshToken).getExpiresAtAsInstant();
                if (expiry != null) {
                    return new ReceivedTokens(accessToken, refreshToken, type, accessTokenExpiresAt, expiry);
                }
                // The JWT carries no expiration date, fall back to the estimates below
            } catch (JWTDecodeException _) {
                // No valid JWT, fall back to implementation from OAuth expires_in or the default value
            }
        }

        // Check if the 'expires in' field, actually meant for the access token, is better than our refresh token
        // default expires value tomorrow.
        if (accessTokenExpiresIn > TWO_DAYS_IN_SECONDS) {
            return new ReceivedTokens(accessToken, refreshToken, type, accessTokenExpiresAt, accessTokenExpiresAt);
        }

        // Use default value tomorrow, we expect a refresh token to be valid at least for one more day
        Instant expiresDate = Instant.now().plus(MINIMUM_REFRESH_EXPIRES_DAYS, ChronoUnit.DAYS);
        return new ReceivedTokens(accessToken, refreshToken, type, accessTokenExpiresAt, expiresDate);
    }
}
