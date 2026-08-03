/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.http

import com.typesafe.config.ConfigFactory
import io.netty.handler.codec.http.HttpHeaderNames
import io.netty.handler.codec.http.HttpMethod
import org.junit.jupiter.api.AfterEach
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.assertAll
import org.junit.jupiter.api.extension.ExtendWith
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.NullSource
import org.junit.jupiter.params.provider.ValueSource
import sirius.kernel.SiriusExtension
import sirius.web.cors.AllowedOrigin
import sirius.web.security.ScopeInfo
import sirius.web.security.UserContext
import java.net.HttpURLConnection
import java.net.URI
import kotlin.test.assertContains
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertNull
import kotlin.test.assertTrue

/**
 * Tests the CORS handling of the web server.
 *
 * Note: Some tests set the restricted `Origin:` header, requiring `-Dsun.net.http.allowRestrictedHeaders=true`. This is
 * enabled centrally in [SiriusExtension.beforeAll].
 */
@ExtendWith(SiriusExtension::class)
class CorsTest {

    private val specificAllowedOrigins = setOf("https://a.example.com", "https://b.example.com")

    @AfterEach
    fun resetInterceptorStrategy() {
        TestCorsInterceptor.allowedOrigin = null
    }

    @Test
    fun `given corsAllowAll is enabled when a request carries an origin then it is reflected as 'Access-Control-Allow-Origin'`() {
        assertEquals("TEST", requestAllowedOrigin("TEST", disableCorsAll = false))
    }

    @Test
    fun `given corsAllowAll is enabled when a preflight request is received then origin, methods and headers are answered`() {
        val connection = sendPreflight(origin = "TEST", requestHeaders = "X-Test")

        assertAll(
            {
                assertEquals(
                    "TEST",
                    connection.getHeaderField(HttpHeaderNames.ACCESS_CONTROL_ALLOW_ORIGIN.toString())
                )
            },
            {
                assertContains(
                    connection.getHeaderField(HttpHeaderNames.ACCESS_CONTROL_ALLOW_METHODS.toString()),
                    "GET"
                )
            },
            {
                assertContains(
                    connection.getHeaderField(HttpHeaderNames.ACCESS_CONTROL_ALLOW_HEADERS.toString()),
                    "X-Test"
                )
            },
        )
    }

    @Test
    fun `given corsAllowAll is enabled when a preflight request is received then 'Access-Control-Allow-Methods' is derived from the registered routes`() {
        // '/test/another-restricted-method' only declares GET, so the preflight must advertise exactly GET and the
        // centrally handled OPTIONS - and not the previously hard-coded "GET,PUT,POST,DELETE".
        val connection = sendPreflight(uri = "/test/another-restricted-method", origin = "TEST")

        val allowedMethods = connection.getHeaderField(HttpHeaderNames.ACCESS_CONTROL_ALLOW_METHODS.toString())
        assertEquals("GET, OPTIONS", allowedMethods)
    }

    @Test
    fun `given a scope overriding corsAllowAll to false when the global setting is enabled then automatic cors is disabled`() {
        UserContext.get().setCurrentScope(configuredScope("corsDisabled", false))
        assertFalse(WebContext.isCorsAllowAll())
    }

    @Test
    fun `given a scope without a corsAllowAll override when the global setting is enabled then the global setting is used`() {
        UserContext.get().setCurrentScope(configuredScope("default", null))
        assertTrue(WebContext.isCorsAllowAll())
    }

    @ParameterizedTest
    @NullSource
    @ValueSource(strings = ["https://example.com", "http://localhost:3000"])
    fun `given corsAllowAll is disabled when the interceptor returns a Wildcard strategy then the asterisk is returned regardless of the requested origin`(
        requestOrigin: String?
    ) {
        TestCorsInterceptor.allowedOrigin = AllowedOrigin.Wildcard()

        assertEquals("*", requestAllowedOrigin(requestOrigin))
    }

    @ParameterizedTest
    @ValueSource(strings = ["https://example.com", "http://localhost:3000", "https://sub.domain.example.com:8443"])
    fun `given corsAllowAll is disabled when the interceptor returns a MatchRequest strategy then the requested origin is reflected`(
        requestOrigin: String
    ) {
        TestCorsInterceptor.allowedOrigin = AllowedOrigin.MatchRequest()

        assertEquals(requestOrigin, requestAllowedOrigin(requestOrigin))
    }

    @ParameterizedTest
    @ValueSource(strings = ["https://a.example.com", "https://b.example.com"])
    fun `given corsAllowAll is disabled when the interceptor returns a Specific strategy then an allowed origin is reflected`(
        requestOrigin: String
    ) {
        TestCorsInterceptor.allowedOrigin = AllowedOrigin.Specific(specificAllowedOrigins)

        assertEquals(requestOrigin, requestAllowedOrigin(requestOrigin))
    }

    @ParameterizedTest
    @ValueSource(strings = ["HTTPS://A.EXAMPLE.COM", "https://evil.example.com"])
    fun `given corsAllowAll is disabled when the interceptor returns a Specific strategy then a disallowed origin yields no 'Access-Control-Allow-Origin' header`(
        requestOrigin: String
    ) {
        TestCorsInterceptor.allowedOrigin = AllowedOrigin.Specific(specificAllowedOrigins)

        assertNull(requestAllowedOrigin(requestOrigin))
    }

    @Test
    fun `given corsAllowAll is enabled when the interceptor returns a restrictive strategy then it is ignored and the requested origin is reflected`() {
        // The interceptor would only allow a different origin, but with corsAllowAll enabled (no scope override) the
        // dispatcher uses MatchRequest and simply reflects the requested origin.
        TestCorsInterceptor.allowedOrigin = AllowedOrigin.Specific(setOf("https://allowed.example.com"))

        assertEquals("https://any.example.com", requestAllowedOrigin("https://any.example.com", disableCorsAll = false))
    }

    @Test
    fun `given corsAllowAll is disabled when the interceptor cannot decide on a strategy then no 'Access-Control-Allow-Origin' header is returned`() {
        TestCorsInterceptor.allowedOrigin = null

        assertNull(requestAllowedOrigin("https://example.com"))
    }

    @Test
    fun `given corsAllowAll is disabled when a preflight request is received then the interceptor strategy is honored`() {
        TestCorsInterceptor.allowedOrigin = AllowedOrigin.Specific(specificAllowedOrigins)

        val connection = sendPreflight(origin = "https://b.example.com", disableCorsAll = true)

        assertEquals(
            "https://b.example.com",
            connection.getHeaderField(HttpHeaderNames.ACCESS_CONTROL_ALLOW_ORIGIN.toString())
        )
    }

    private fun configuredScope(scopeType: String, corsAllowAll: Boolean?): ScopeInfo =
        ScopeInfo(
            scopeType,
            scopeType,
            scopeType,
            null,
            corsAllowAll?.let { value -> { ConfigFactory.parseString("http.corsAllowAll=$value") } },
            null
        )

    /**
     * Sends a GET request to `/system/ok` and returns the resulting `Access-Control-Allow-Origin` response header, or
     * `null` if none was set.
     *
     * If [requestOrigin] is given, it is sent as the `Origin` header. If [disableCorsAll] is set, the request is bound
     * (via [TestCorsScopeDetector]) to a scope which disables the global CORS handling, so that the
     * [TestCorsInterceptor] strategy is used to resolve the allowed origin.
     */
    private fun requestAllowedOrigin(requestOrigin: String?, disableCorsAll: Boolean = true): String? {
        val connection = URI("http://localhost:9999/system/ok").toURL().openConnection() as HttpURLConnection
        if (requestOrigin != null) {
            connection.addRequestProperty(HttpHeaderNames.ORIGIN.toString(), requestOrigin)
        }
        if (disableCorsAll) {
            connection.addRequestProperty(TestCorsScopeDetector.HEADER_DISABLE_CORS_ALL, "true")
        }
        connection.inputStream.close()
        return connection.getHeaderField(HttpHeaderNames.ACCESS_CONTROL_ALLOW_ORIGIN.toString())
    }

    /**
     * Sends a CORS preflight (`OPTIONS`) request to [uri] and returns the connection, so that the response headers can
     * be inspected.
     *
     * If [disableCorsAll] is set, the request is bound (via [TestCorsScopeDetector]) to a scope which disables the
     * global CORS handling, so that the [TestCorsInterceptor] strategy is used to resolve the allowed origin.
     */
    private fun sendPreflight(
        origin: String,
        uri: String = "/system/ok",
        requestMethod: String = HttpMethod.GET.name(),
        requestHeaders: String? = null,
        disableCorsAll: Boolean = false
    ): HttpURLConnection {
        val connection = URI("http://localhost:9999$uri").toURL().openConnection() as HttpURLConnection
        connection.requestMethod = HttpMethod.OPTIONS.name()
        connection.addRequestProperty(HttpHeaderNames.ORIGIN.toString(), origin)
        connection.addRequestProperty(HttpHeaderNames.ACCESS_CONTROL_REQUEST_METHOD.toString(), requestMethod)
        if (requestHeaders != null) {
            connection.addRequestProperty(HttpHeaderNames.ACCESS_CONTROL_REQUEST_HEADERS.toString(), requestHeaders)
        }
        if (disableCorsAll) {
            connection.addRequestProperty(TestCorsScopeDetector.HEADER_DISABLE_CORS_ALL, "true")
        }
        connection.inputStream.close()
        return connection
    }
}
