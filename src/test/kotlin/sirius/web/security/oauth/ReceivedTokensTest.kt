/*
 * Made with all the love in the world
 * by scireum in Stuttgart, Germany
 *
 * Copyright by scireum GmbH
 * https://www.scireum.de - info@scireum.de
 */

package sirius.web.security.oauth

import com.auth0.jwt.JWT
import com.auth0.jwt.algorithms.Algorithm
import org.junit.jupiter.api.Test
import sirius.kernel.commons.Json
import tools.jackson.databind.node.ObjectNode
import java.time.Duration
import java.time.Instant
import java.util.TimeZone
import kotlin.test.assertEquals
import kotlin.test.assertNull
import kotlin.test.assertTrue

/**
 * Provides some basic tests for the [ReceivedTokens], mainly covering how the expiry of a refresh token is
 * determined.
 */
class ReceivedTokensTest {

    @Test
    fun `reads refresh token expiry from JWT bearer token`() {
        val expiry = Instant.parse("2039-01-15T12:34:56Z")
        val response = bearerResponse().put(OAuth.REFRESH_TOKEN, jwtExpiringAt(expiry)).put(OAuth.EXPIRES_IN, 3600)

        val tokens = ReceivedTokens.fromJson(response)

        assertEquals("the-access-token", tokens.accessToken())
        assertEquals(OAuth.TOKEN_TYPE_BEARER, tokens.type())
        assertEquals(expiry, tokens.refreshTokenExpiresAt())
        assertCloseToNowPlusSeconds(3600, tokens.accessTokenExpiresAt())
    }

    @Test
    fun `reads refresh token expiry independently of the system time zone`() {
        val expiry = Instant.parse("2039-01-15T12:34:56Z")
        val originalTimeZone = TimeZone.getDefault()
        try {
            TimeZone.setDefault(TimeZone.getTimeZone("America/New_York"))
            val response = bearerResponse().put(OAuth.REFRESH_TOKEN, jwtExpiringAt(expiry))

            val tokens = ReceivedTokens.fromJson(response)

            assertEquals(expiry, tokens.refreshTokenExpiresAt())
        } finally {
            TimeZone.setDefault(originalTimeZone)
        }
    }

    @Test
    fun `ignores JWT refresh token of other token types`() {
        val expiry = Instant.parse("2039-01-15T12:34:56Z")
        val response = Json.createObject()
            .put(OAuth.ACCESS_TOKEN, "the-access-token")
            .put(OAuth.TOKEN_TYPE, "MAC")
            .put(OAuth.REFRESH_TOKEN, jwtExpiringAt(expiry))
            .put(OAuth.EXPIRES_IN, 3600)

        val tokens = ReceivedTokens.fromJson(response)

        assertCloseToNowPlusSeconds(Duration.ofDays(1).toSeconds(), tokens.refreshTokenExpiresAt())
    }

    @Test
    fun `falls back to tomorrow for JWT refresh tokens without expiry`() {
        val jwtWithoutExpiry = JWT.create().sign(Algorithm.none())
        val response = bearerResponse().put(OAuth.REFRESH_TOKEN, jwtWithoutExpiry).put(OAuth.EXPIRES_IN, 3600)

        val tokens = ReceivedTokens.fromJson(response)

        assertEquals(jwtWithoutExpiry, tokens.refreshToken())
        assertCloseToNowPlusSeconds(Duration.ofDays(1).toSeconds(), tokens.refreshTokenExpiresAt())
    }

    @Test
    fun `falls back to tomorrow for opaque refresh tokens`() {
        val response =
            bearerResponse().put(OAuth.REFRESH_TOKEN, "an-opaque-refresh-token").put(OAuth.EXPIRES_IN, 3600)

        val tokens = ReceivedTokens.fromJson(response)

        assertEquals("an-opaque-refresh-token", tokens.refreshToken())
        assertCloseToNowPlusSeconds(Duration.ofDays(1).toSeconds(), tokens.refreshTokenExpiresAt())
    }

    @Test
    fun `prefers long access token expiry over the default of tomorrow`() {
        val expiresIn = Duration.ofDays(2).toSeconds() + 1
        val response =
            bearerResponse().put(OAuth.REFRESH_TOKEN, "an-opaque-refresh-token").put(OAuth.EXPIRES_IN, expiresIn)

        val tokens = ReceivedTokens.fromJson(response)

        assertEquals(tokens.accessTokenExpiresAt(), tokens.refreshTokenExpiresAt())
        assertCloseToNowPlusSeconds(expiresIn, tokens.refreshTokenExpiresAt())
    }

    @Test
    fun `treats an access token expiry of exactly two days as too short`() {
        val expiresIn = Duration.ofDays(2).toSeconds()
        val response =
            bearerResponse().put(OAuth.REFRESH_TOKEN, "an-opaque-refresh-token").put(OAuth.EXPIRES_IN, expiresIn)

        val tokens = ReceivedTokens.fromJson(response)

        assertCloseToNowPlusSeconds(Duration.ofDays(1).toSeconds(), tokens.refreshTokenExpiresAt())
    }

    @Test
    fun `reports no refresh token expiry without refresh token`() {
        val response = bearerResponse().put(OAuth.EXPIRES_IN, 3600)

        val tokens = ReceivedTokens.fromJson(response)

        assertNull(tokens.refreshToken())
        assertNull(tokens.refreshTokenExpiresAt())
        assertCloseToNowPlusSeconds(3600, tokens.accessTokenExpiresAt())
    }

    @Test
    fun `reports no access token expiry without expires in`() {
        val response = bearerResponse()

        val tokens = ReceivedTokens.fromJson(response)

        assertNull(tokens.accessTokenExpiresAt())
        assertNull(tokens.refreshTokenExpiresAt())
    }

    /**
     * Creates a minimal response of an authorization server, containing an access token of type "Bearer".
     *
     * @return the response which further fields can be added to
     */
    private fun bearerResponse(): ObjectNode {
        return Json.createObject()
            .put(OAuth.ACCESS_TOKEN, "the-access-token")
            .put(OAuth.TOKEN_TYPE, OAuth.TOKEN_TYPE_BEARER)
    }

    /**
     * Creates a JWT which expires at the given instant. As the token is only decoded and never verified, it doesn't
     * need to carry a proper signature.
     *
     * @param expiry the instant at which the token expires
     * @return the encoded JWT
     */
    private fun jwtExpiringAt(expiry: Instant): String {
        return JWT.create().withExpiresAt(expiry).sign(Algorithm.none())
    }

    /**
     * Asserts that the given date lies the expected number of seconds in the future, tolerating the time elapsed
     * between computing the date and checking it.
     *
     * @param seconds the number of seconds the given date is expected to lie in the future
     * @param actual  the date to check
     */
    private fun assertCloseToNowPlusSeconds(seconds: Long, actual: Instant?) {
        val deviation = Duration.between(Instant.now().plusSeconds(seconds), actual).abs()
        assertTrue(
            deviation < MAXIMUM_DEVIATION,
            "Expected a date in ${seconds}s, but was off by $deviation: $actual"
        )
    }

    companion object {
        private val MAXIMUM_DEVIATION: Duration = Duration.ofSeconds(10)
    }
}
