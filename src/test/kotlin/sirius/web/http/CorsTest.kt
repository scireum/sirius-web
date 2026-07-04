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
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.extension.ExtendWith
import sirius.kernel.SiriusExtension
import sirius.web.security.ScopeInfo
import sirius.web.security.UserContext
import java.net.HttpURLConnection
import java.net.URI
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertTrue

/**
 * Tests the CORS handling of the web server.
 *
 * Note: Some tests set the restricted `Origin:` header, requiring `-Dsun.net.http.allowRestrictedHeaders=true`. This is
 * enabled centrally in [SiriusExtension.beforeAll].
 */
@ExtendWith(SiriusExtension::class)
class CorsTest {
    @Test
    fun `expect 'Access-Control-Allow-Origin' for requests with 'origin'`() {
        val connection = URI("http://localhost:9999/system/ok").toURL().openConnection() as HttpURLConnection
        connection.addRequestProperty("Origin", "TEST")
        connection.getInputStream().close()
        assertEquals("TEST", connection.getHeaderField(HttpHeaderNames.ACCESS_CONTROL_ALLOW_ORIGIN.toString()))
    }

    @Test
    fun `expect a CORS preflight request to be answered correctly`() {
        val connection = URI("http://localhost:9999/system/ok").toURL().openConnection() as HttpURLConnection
        connection.setRequestMethod(HttpMethod.OPTIONS.name())
        connection.addRequestProperty("Origin", "TEST")
        connection.addRequestProperty("Access-Control-Request-Method", "GET")
        connection.addRequestProperty("Access-Control-Request-Headers", "X-Test")

        connection.getInputStream().close()
        assertEquals("TEST", connection.getHeaderField(HttpHeaderNames.ACCESS_CONTROL_ALLOW_ORIGIN.toString()))
        assertTrue {
            connection.getHeaderField(HttpHeaderNames.ACCESS_CONTROL_ALLOW_METHODS.toString()).contains("GET")
        }
        assertTrue {
            connection.getHeaderField(HttpHeaderNames.ACCESS_CONTROL_ALLOW_HEADERS.toString()).contains("X-Test")
        }
    }

    @Test
    fun `preflight 'Access-Control-Allow-Methods' is derived from the registered routes`() {
        // '/test/another-restricted-method' only declares GET, so the preflight must advertise exactly GET and the
        // centrally handled OPTIONS - and not the previously hard-coded "GET,PUT,POST,DELETE".
        val connection =
            URI("http://localhost:9999/test/another-restricted-method").toURL().openConnection() as HttpURLConnection
        connection.setRequestMethod(HttpMethod.OPTIONS.name())
        connection.addRequestProperty("Origin", "TEST")
        connection.addRequestProperty("Access-Control-Request-Method", "GET")

        connection.getInputStream().close()
        val allowedMethods = connection.getHeaderField(HttpHeaderNames.ACCESS_CONTROL_ALLOW_METHODS.toString())
        assertEquals("GET, OPTIONS", allowedMethods)
    }

    @Test
    fun `configured scope disables automatic cors even if global setting is enabled`() {
        UserContext.get().setCurrentScope(configuredScope("corsDisabled", false))
        assertFalse(WebContext.isCorsAllowAll())
    }

    @Test
    fun `global cors setting is used if scope does not define an override`() {
        UserContext.get().setCurrentScope(configuredScope("default", null))
        assertTrue(WebContext.isCorsAllowAll())
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
}
