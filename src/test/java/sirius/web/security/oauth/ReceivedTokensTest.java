/*
 * Made with all the love in the world
 * by scireum in Stuttgart, Germany
 *
 * Copyright by scireum GmbH
 * https://www.scireum.de - info@scireum.de
 */

package sirius.web.security.oauth;

import com.auth0.jwt.JWT;
import com.auth0.jwt.algorithms.Algorithm;
import org.junit.jupiter.api.Test;
import sirius.kernel.commons.Json;
import tools.jackson.databind.node.ObjectNode;

import java.time.Duration;
import java.time.Instant;
import java.util.TimeZone;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Provides some basic tests for the {@link ReceivedTokens}, mainly covering how the expiry of a refresh token is
 * determined.
 */
public class ReceivedTokensTest {

    private static final Duration MAXIMUM_DEVIATION = Duration.ofSeconds(10);

    @Test
    public void readsRefreshTokenExpiryFromJwtBearerToken() {
        Instant expiry = Instant.parse("2039-01-15T12:34:56Z");
        ObjectNode response =
                bearerResponse().put(OAuth.REFRESH_TOKEN, jwtExpiringAt(expiry)).put(OAuth.EXPIRES_IN, 3600);

        ReceivedTokens tokens = ReceivedTokens.fromJson(response);

        assertEquals("the-access-token", tokens.accessToken());
        assertEquals(OAuth.TOKEN_TYPE_BEARER, tokens.type());
        assertEquals(expiry, tokens.refreshTokenExpiresAt());
        assertCloseToNowPlusSeconds(3600, tokens.accessTokenExpiresAt());
    }

    @Test
    public void readsRefreshTokenExpiryIndependentlyOfTheSystemTimeZone() {
        Instant expiry = Instant.parse("2039-01-15T12:34:56Z");
        TimeZone originalTimeZone = TimeZone.getDefault();
        try {
            TimeZone.setDefault(TimeZone.getTimeZone("America/New_York"));
            ObjectNode response = bearerResponse().put(OAuth.REFRESH_TOKEN, jwtExpiringAt(expiry));

            ReceivedTokens tokens = ReceivedTokens.fromJson(response);

            assertEquals(expiry, tokens.refreshTokenExpiresAt());
        } finally {
            TimeZone.setDefault(originalTimeZone);
        }
    }

    @Test
    public void ignoresJwtRefreshTokenOfOtherTokenTypes() {
        Instant expiry = Instant.parse("2039-01-15T12:34:56Z");
        ObjectNode response = Json.createObject()
                                  .put(OAuth.ACCESS_TOKEN, "the-access-token")
                                  .put(OAuth.TOKEN_TYPE, "MAC")
                                  .put(OAuth.REFRESH_TOKEN, jwtExpiringAt(expiry))
                                  .put(OAuth.EXPIRES_IN, 3600);

        ReceivedTokens tokens = ReceivedTokens.fromJson(response);

        assertCloseToNowPlusSeconds(Duration.ofDays(1).toSeconds(), tokens.refreshTokenExpiresAt());
    }

    @Test
    public void fallsBackToTomorrowForJwtRefreshTokensWithoutExpiry() {
        String jwtWithoutExpiry = JWT.create().sign(Algorithm.none());
        ObjectNode response = bearerResponse().put(OAuth.REFRESH_TOKEN, jwtWithoutExpiry).put(OAuth.EXPIRES_IN, 3600);

        ReceivedTokens tokens = ReceivedTokens.fromJson(response);

        assertEquals(jwtWithoutExpiry, tokens.refreshToken());
        assertCloseToNowPlusSeconds(Duration.ofDays(1).toSeconds(), tokens.refreshTokenExpiresAt());
    }

    @Test
    public void fallsBackToTomorrowForOpaqueRefreshTokens() {
        ObjectNode response =
                bearerResponse().put(OAuth.REFRESH_TOKEN, "an-opaque-refresh-token").put(OAuth.EXPIRES_IN, 3600);

        ReceivedTokens tokens = ReceivedTokens.fromJson(response);

        assertEquals("an-opaque-refresh-token", tokens.refreshToken());
        assertCloseToNowPlusSeconds(Duration.ofDays(1).toSeconds(), tokens.refreshTokenExpiresAt());
    }

    @Test
    public void prefersLongAccessTokenExpiryOverTheDefaultOfTomorrow() {
        long expiresIn = Duration.ofDays(2).toSeconds() + 1;
        ObjectNode response =
                bearerResponse().put(OAuth.REFRESH_TOKEN, "an-opaque-refresh-token").put(OAuth.EXPIRES_IN, expiresIn);

        ReceivedTokens tokens = ReceivedTokens.fromJson(response);

        assertEquals(tokens.accessTokenExpiresAt(), tokens.refreshTokenExpiresAt());
        assertCloseToNowPlusSeconds(expiresIn, tokens.refreshTokenExpiresAt());
    }

    @Test
    public void treatsAnAccessTokenExpiryOfExactlyTwoDaysAsTooShort() {
        long expiresIn = Duration.ofDays(2).toSeconds();
        ObjectNode response =
                bearerResponse().put(OAuth.REFRESH_TOKEN, "an-opaque-refresh-token").put(OAuth.EXPIRES_IN, expiresIn);

        ReceivedTokens tokens = ReceivedTokens.fromJson(response);

        assertCloseToNowPlusSeconds(Duration.ofDays(1).toSeconds(), tokens.refreshTokenExpiresAt());
    }

    @Test
    public void reportsNoRefreshTokenExpiryWithoutRefreshToken() {
        ObjectNode response = bearerResponse().put(OAuth.EXPIRES_IN, 3600);

        ReceivedTokens tokens = ReceivedTokens.fromJson(response);

        assertNull(tokens.refreshToken());
        assertNull(tokens.refreshTokenExpiresAt());
        assertCloseToNowPlusSeconds(3600, tokens.accessTokenExpiresAt());
    }

    @Test
    public void reportsNoAccessTokenExpiryWithoutExpiresIn() {
        ObjectNode response = bearerResponse();

        ReceivedTokens tokens = ReceivedTokens.fromJson(response);

        assertNull(tokens.accessTokenExpiresAt());
        assertNull(tokens.refreshTokenExpiresAt());
    }

    /**
     * Creates a minimal response of an authorization server, containing an access token of type "Bearer".
     *
     * @return the response which further fields can be added to
     */
    private static ObjectNode bearerResponse() {
        return Json.createObject()
                   .put(OAuth.ACCESS_TOKEN, "the-access-token")
                   .put(OAuth.TOKEN_TYPE, OAuth.TOKEN_TYPE_BEARER);
    }

    /**
     * Creates a JWT which expires at the given instant. As the token is only decoded and never verified, it doesn't
     * need to carry a proper signature.
     *
     * @param expiry the instant at which the token expires
     * @return the encoded JWT
     */
    private static String jwtExpiringAt(Instant expiry) {
        return JWT.create().withExpiresAt(expiry).sign(Algorithm.none());
    }

    /**
     * Asserts that the given date lies the expected number of seconds in the future, tolerating the time elapsed
     * between computing the date and checking it.
     *
     * @param seconds the number of seconds the given date is expected to lie in the future
     * @param actual  the date to check
     */
    private static void assertCloseToNowPlusSeconds(long seconds, Instant actual) {
        Duration deviation = Duration.between(Instant.now().plusSeconds(seconds), actual).abs();
        assertTrue(deviation.compareTo(MAXIMUM_DEVIATION) < 0,
                   "Expected a date in " + seconds + "s, but was off by " + deviation + ": " + actual);
    }
}
