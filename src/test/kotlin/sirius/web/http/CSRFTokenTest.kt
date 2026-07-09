/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.http

import io.netty.handler.codec.http.HttpHeaderNames
import io.netty.handler.codec.http.HttpResponseStatus
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.extension.ExtendWith
import sirius.kernel.SiriusExtension
import sirius.kernel.commons.Streams
import java.net.HttpURLConnection
import java.net.URI
import java.nio.charset.StandardCharsets
import kotlin.test.assertEquals

@ExtendWith(SiriusExtension::class)
class CSRFTokenTest {

    @Test
    fun `Routed POST fails if token is missing via POST`() {

        val result = TestRequest.POST("/test/fake-delete-data").execute()

        assertEquals(HttpResponseStatus.FORBIDDEN, result.status)
    }

    @Test
    fun `Routed POST works on SAFEPOST if correct token is provided`() {

        val result = TestRequest.SAFEPOST("/test/fake-delete-data").execute()

        assertEquals(HttpResponseStatus.OK, result.status)
    }

    @Test
    fun `Routed POST works if correct token is present via POST`() {

        val connection =
            URI("http://localhost:9999/test/provide-security-token").toURL().openConnection() as HttpURLConnection
        connection.requestMethod = "GET"
        connection.connect()
        val token = String(Streams.toByteArray(connection.inputStream), StandardCharsets.UTF_8)


        val connection2 = URI(
            "http://localhost:9999/test/fake-delete-data?CSRFToken=$token"
        ).toURL().openConnection() as HttpURLConnection
        connection2.requestMethod = "POST"
        connection2.setRequestProperty(
            HttpHeaderNames.COOKIE.toString(),
            connection.headerFields["set-cookie"]?.get(0)
        )
        connection2.connect()

        assertEquals(200, connection2.responseCode)
    }

    @Test
    fun `Routed POST accepts previous token after token rotation`() {

        val connection =
            URI("http://localhost:9999/test/provide-security-token").toURL().openConnection() as HttpURLConnection
        connection.requestMethod = "GET"
        connection.connect()
        val token = String(Streams.toByteArray(connection.inputStream), StandardCharsets.UTF_8)
        TestRequest.GET("/test/expire-security-token").execute()

        val connection2 = URI(
            "http://localhost:9999/test/fake-delete-data?CSRFToken=$token"
        ).toURL().openConnection() as HttpURLConnection
        connection2.requestMethod = "POST"
        connection2.setRequestProperty(
            HttpHeaderNames.COOKIE.toString(),
            connection.headerFields["set-cookie"]?.get(0)
        )
        connection2.connect()

        assertEquals(200, connection2.responseCode)
    }

    @Test
    fun `Routed POST fails if false token is given via POST`() {

        val connection =
            URI("http://localhost:9999/test/provide-security-token").toURL().openConnection() as HttpURLConnection
        connection.requestMethod = "GET"
        connection.connect()

        val connection2 =
            URI("http://localhost:9999/test/fake-delete-data?CSRFToken=w-r-o-n-g-t-o-k-e-n").toURL()
                .openConnection() as HttpURLConnection
        connection2.requestMethod = "POST"
        connection2.setRequestProperty(
            HttpHeaderNames.COOKIE.toString(),
            connection.headerFields["set-cookie"]?.get(0)
        )
        connection2.connect()

        assertEquals(403, connection2.responseCode)
    }

    @Test
    fun `unsafePOST() if token is missing via POST it works as intended`() {

        val result = TestRequest.POST("/test/fake-delete-data-unsafe").execute()

        assertEquals(HttpResponseStatus.OK, result.status)
    }

    @Test
    fun `unsafePOST() fails on GET if token is not provided`() {

        val result = TestRequest.GET("/test/fake-delete-data-unsafe").execute()

        assertEquals(HttpResponseStatus.INTERNAL_SERVER_ERROR, result.status)
    }

    @Test
    fun `Routed POST fails without CSRF token by default`() {

        val result = TestRequest.POST("/test/routed-ensure-safe-post").execute()

        assertEquals(HttpResponseStatus.FORBIDDEN, result.status)
    }

    @Test
    fun `Routed POST works on SAFEPOST by default`() {

        val result = TestRequest.SAFEPOST("/test/routed-ensure-safe-post").execute()

        assertEquals(HttpResponseStatus.OK, result.status)
    }

    @Test
    fun `Routed GET allows request without CSRF token`() {

        val result = TestRequest.GET("/test/routed-split").execute()

        assertEquals(HttpResponseStatus.OK, result.status)
        assertEquals("GET", result.contentAsString)
    }

    @Test
    fun `Routed POST rejects request without CSRF token on split route`() {

        val result = TestRequest.POST("/test/routed-split").execute()

        assertEquals(HttpResponseStatus.FORBIDDEN, result.status)
    }

    @Test
    fun `Routed POST accepts request with CSRF token on split route`() {

        val result = TestRequest.SAFEPOST("/test/routed-split").execute()

        assertEquals(HttpResponseStatus.OK, result.status)
        assertEquals("POST", result.contentAsString)
    }

    @Test
    fun `Routed skipCsrfValidation allows POST without CSRF token`() {

        val result = TestRequest.POST("/test/routed-skip-csrf").execute()

        assertEquals(HttpResponseStatus.OK, result.status)
    }

    @Test
    fun `Controller isSkipCsrfValidation allows POST without CSRF token`() {

        val result = TestRequest.POST("/test/controller-skip-csrf").execute()

        assertEquals(HttpResponseStatus.OK, result.status)
    }

    @Test
    fun `Controller isSkipCsrfValidation allows POST even with invalid token`() {

        val result = TestRequest.POST("/test/controller-skip-csrf?CSRFToken=invalid").execute()

        assertEquals(HttpResponseStatus.OK, result.status)
    }

    @Test
    fun `Controller isSkipCsrfValidation does not skip CSRF validation on other controllers`() {

        val result = TestRequest.POST("/test/fake-delete-data").execute()

        assertEquals(HttpResponseStatus.FORBIDDEN, result.status)
    }

    @Test
    fun `Controller isSkipCsrfValidation skips all routes of the controller`() {

        val withoutToken = TestRequest.POST("/test/controller-skip-csrf/explicit-route-skip").execute()
        val withInvalidToken =
            TestRequest.POST("/test/controller-skip-csrf/explicit-route-skip?CSRFToken=invalid").execute()

        assertEquals(HttpResponseStatus.OK, withoutToken.status)
        assertEquals(HttpResponseStatus.OK, withInvalidToken.status)
    }
}
