/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.http

import io.netty.handler.codec.http.HttpHeaderNames
import io.netty.handler.codec.http.HttpMethod
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.extension.ExtendWith
import sirius.kernel.BaseSpecification
import sirius.kernel.SiriusExtension
import java.net.HttpURLConnection
import java.net.URI
import java.net.URL
import kotlin.test.assertEquals
import kotlin.test.assertTrue

@ExtendWith(SiriusExtension::class)
class CORSTest {
    @Test
    fun `expect 'Access-Control-Allow-Origin' for requests with 'origin'`() {
        // Allow us to set the Origin: header...
        System.setProperty("sun.net.http.allowRestrictedHeaders", "true")
        val connection = URI("http://localhost:9999/system/ok").toURL().openConnection() as HttpURLConnection
        // Setting the "Origin: header" must be allowed by -Dsun.net.http.allowRestrictedHeaders=true
        connection.addRequestProperty("Origin", "TEST")
        connection.getInputStream().close()
        assertEquals("TEST", connection.getHeaderField(HttpHeaderNames.ACCESS_CONTROL_ALLOW_ORIGIN.toString()))
    }

    @Test
    fun `expect a CORS preflight request to be answered correctly`() {
        // Allow us to set the Origin: header...
        System.setProperty("sun.net.http.allowRestrictedHeaders", "true")
        val connection = URI("http://localhost:9999/system/ok").toURL().openConnection() as HttpURLConnection
        connection.setRequestMethod(HttpMethod.OPTIONS.name())
        connection.addRequestProperty("Origin", "TEST")
        connection.addRequestProperty("Access-Control-Request-Method", "GET")
        connection.addRequestProperty("Access-Control-Request-Headers", "X-Test")

        connection.getInputStream().close()
        assertEquals("TEST", connection.getHeaderField(HttpHeaderNames.ACCESS_CONTROL_ALLOW_ORIGIN.toString()))
        assertTrue { connection.getHeaderField(HttpHeaderNames.ACCESS_CONTROL_ALLOW_METHODS.toString()).indexOf("GET") >= 0 }
        assertTrue { connection.getHeaderField(HttpHeaderNames.ACCESS_CONTROL_ALLOW_HEADERS.toString()).indexOf("X-Test") >= 0 }
    }

}
