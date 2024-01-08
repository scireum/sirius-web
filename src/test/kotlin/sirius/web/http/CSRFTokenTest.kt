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
import java.net.URL

import java.nio.charset.StandardCharsets
import kotlin.test.assertEquals

@ExtendWith(SiriusExtension::class)
class CSRFTokenTest {

    @Test
    fun `safePOST() works correctly if token is missing via GET`() {

        val result = TestRequest.GET("/test/fake-delete-data").execute()

        assertEquals(result.getStatus(), HttpResponseStatus.INTERNAL_SERVER_ERROR)
    }

    @Test
    fun `safePOST() works correctly if token is present via GET`() {

        val connection = URL("http://localhost:9999/test/provide-security-token").openConnection() as HttpURLConnection
        connection.setRequestMethod("GET")
        connection.connect()
        val token = String(Streams.toByteArray(connection.getInputStream()), StandardCharsets.UTF_8)

        val result = TestRequest.GET("/test/fake-delete-data?CSRFToken=$token").execute()


        assertEquals(result.getStatus(), HttpResponseStatus.INTERNAL_SERVER_ERROR)
    }

    @Test
    fun `safePOST() works correctly if token is missing via POST`() {

        val result = TestRequest.POST("/test/fake-delete-data").execute()
        assertEquals(result.getStatus(), HttpResponseStatus.INTERNAL_SERVER_ERROR)

    }

    @Test
    fun `safePOST() works correctly if correct token is present via SAFEPOST`() {

        val result = TestRequest.SAFEPOST("/test/fake-delete-data").execute()

        assertEquals(result.getStatus(), HttpResponseStatus.OK)
    }

    @Test
    fun `safePOST() works correctly if correct token is present via POST`() {

        val connection = URL("http://localhost:9999/test/provide-security-token").openConnection() as HttpURLConnection
        connection.setRequestMethod("GET")
        connection.connect()
        val token = String(Streams.toByteArray(connection.getInputStream()), StandardCharsets.UTF_8)


        val connection2 = URL(
            "http://localhost:9999/test/fake-delete-data?CSRFToken=$token"
        ).openConnection() as HttpURLConnection
        connection2.setRequestMethod("POST")
        connection2.setRequestProperty(
            HttpHeaderNames.COOKIE.toString(),
            connection.getHeaderFields().get("set-cookie")?.get(0)
        )
        connection2.connect()

        assertEquals(connection2.getResponseCode(), 200)
    }

    @Test
    fun `safePOST() works correctly if expired token is present via POST`() {

        val connection = URL("http://localhost:9999/test/provide-security-token").openConnection() as HttpURLConnection
        connection.setRequestMethod("GET")
        connection.connect()
        val token = String(Streams.toByteArray(connection.getInputStream()), StandardCharsets.UTF_8)
        TestRequest.GET("/test/expire-security-token").execute()

        val connection2 = URL(
            "http://localhost:9999/test/fake-delete-data?CSRFToken=$token"
        ).openConnection() as HttpURLConnection
        connection2.setRequestMethod("POST")
        connection2.setRequestProperty(
            HttpHeaderNames.COOKIE.toString(),
            connection.getHeaderFields().get("set-cookie")?.get(0)
        )
        connection2.connect()

        assertEquals(connection2.getResponseCode(), 200)
    }

    @Test
    fun `safePOST() works correctly if wrong token is present via POST`() {

        val connection = URL("http://localhost:9999/test/provide-security-token").openConnection() as HttpURLConnection
        connection.setRequestMethod("GET")
        connection.connect()

        val connection2 =
            URL("http://localhost:9999/test/fake-delete-data?CSRFToken=w-r-o-n-g-t-o-k-e-n").openConnection() as HttpURLConnection
        connection2.setRequestMethod("POST")
        connection2.setRequestProperty(
            HttpHeaderNames.COOKIE.toString(),
            connection.getHeaderFields().get("set-cookie")?.get(0)
        )
        connection2.connect()

        assertEquals(connection2.getResponseCode(), 500)
    }

    @Test
    fun `unsafePOST() works correctly if token is missing via POST`() {

        val result = TestRequest.POST("/test/fake-delete-data-unsafe").execute()

        assertEquals(result.getStatus(), HttpResponseStatus.OK)
    }

    @Test
    fun `unsafePOST() works correctly if token is missing via GET`() {

        val result = TestRequest.GET("/test/fake-delete-data-unsafe").execute()

        assertEquals(result.getStatus(), HttpResponseStatus.INTERNAL_SERVER_ERROR)
    }

    @Test
    fun `ensureSafePOST() works correctly if token is missing via GET`() {

        val result = TestRequest.GET("/test/fake-delete-data-ensure-safe").execute()

        assertEquals(result.getStatus(), HttpResponseStatus.INTERNAL_SERVER_ERROR)
    }

    @Test
    fun `ensureSafePOST() works correctly if wrong token is present via POST`() {

        val connection = URL(
            "http://localhost:9999/test/fake-delete-data-ensure-safe?CSRFToken=w-r-o-n-g-t-o-k-e-n"
        ).openConnection() as HttpURLConnection
        connection.setRequestMethod("POST")
        connection.connect()

        assertEquals(connection.responseCode, 401)
    }

    @Test
    fun `ensureSafePOST() works correctly if correct token is present via POST`() {

        val connection = URL("http://localhost:9999/test/provide-security-token").openConnection() as HttpURLConnection
        connection.setRequestMethod("GET")
        connection.connect()
        val token = String(Streams.toByteArray(connection.getInputStream()), StandardCharsets.UTF_8)

        val connection2 =
            URL("http://localhost:9999/test/fake-delete-data-ensure-safe?CSRFToken=$token").openConnection() as HttpURLConnection
        connection2.setRequestMethod("POST")
        connection2.setRequestProperty(
            HttpHeaderNames.COOKIE.toString(),
            connection.getHeaderFields().get("set-cookie")?.get(0)
        )
        connection2.connect()

        assertEquals(connection2.getResponseCode(), 200)
    }
}
