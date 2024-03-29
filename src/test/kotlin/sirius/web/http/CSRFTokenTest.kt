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
    fun `safePOST() fails if token is missing via GET`() {

        val result = TestRequest.GET("/test/fake-delete-data").execute()

        assertEquals(HttpResponseStatus.INTERNAL_SERVER_ERROR, result.status)
    }

    @Test
    fun `safePOST() works if the token is present via GET`() {

        val connection = URL("http://localhost:9999/test/provide-security-token").openConnection() as HttpURLConnection
        connection.setRequestMethod("GET")
        connection.connect()
        val token = String(Streams.toByteArray(connection.inputStream), StandardCharsets.UTF_8)

        val result = TestRequest.GET("/test/fake-delete-data?CSRFToken=$token").execute()

        assertEquals(HttpResponseStatus.INTERNAL_SERVER_ERROR, result.status)
    }

    @Test
    fun `safePOST() fails if token is missing via POST`() {

        val result = TestRequest.POST("/test/fake-delete-data").execute()

        assertEquals(HttpResponseStatus.INTERNAL_SERVER_ERROR, result.status)
    }

    @Test
    fun `safePOST() works on SAFEPOST if correct token is provided`() {

        val result = TestRequest.SAFEPOST("/test/fake-delete-data").execute()

        assertEquals(HttpResponseStatus.OK, result.status)
    }

    @Test
    fun `safePOST() works if correct token is present via POST`() {

        val connection = URL("http://localhost:9999/test/provide-security-token").openConnection() as HttpURLConnection
        connection.setRequestMethod("GET")
        connection.connect()
        val token = String(Streams.toByteArray(connection.inputStream), StandardCharsets.UTF_8)


        val connection2 = URL(
            "http://localhost:9999/test/fake-delete-data?CSRFToken=$token"
        ).openConnection() as HttpURLConnection
        connection2.setRequestMethod("POST")
        connection2.setRequestProperty(
            HttpHeaderNames.COOKIE.toString(),
            connection.headerFields["set-cookie"]?.get(0)
        )
        connection2.connect()

        assertEquals(200, connection2.getResponseCode())
    }

    @Test
    fun `safePOST() success on POST if expired token is provided`() {

        val connection = URL("http://localhost:9999/test/provide-security-token").openConnection() as HttpURLConnection
        connection.setRequestMethod("GET")
        connection.connect()
        val token = String(Streams.toByteArray(connection.inputStream), StandardCharsets.UTF_8)
        TestRequest.GET("/test/expire-security-token").execute()

        val connection2 = URL(
            "http://localhost:9999/test/fake-delete-data?CSRFToken=$token"
        ).openConnection() as HttpURLConnection
        connection2.setRequestMethod("POST")
        connection2.setRequestProperty(
            HttpHeaderNames.COOKIE.toString(),
            connection.headerFields["set-cookie"]?.get(0)
        )
        connection2.connect()

        assertEquals(200,connection2.getResponseCode())
    }

    @Test
    fun `safePOST() works if false token is given via POST`() {

        val connection = URL("http://localhost:9999/test/provide-security-token").openConnection() as HttpURLConnection
        connection.setRequestMethod("GET")
        connection.connect()

        val connection2 =
            URL("http://localhost:9999/test/fake-delete-data?CSRFToken=w-r-o-n-g-t-o-k-e-n").openConnection() as HttpURLConnection
        connection2.setRequestMethod("POST")
        connection2.setRequestProperty(
            HttpHeaderNames.COOKIE.toString(),
            connection.headerFields["set-cookie"]?.get(0)
        )
        connection2.connect()

        assertEquals(500,connection2.getResponseCode())
    }

    @Test
    fun `unsafePOST() if token is missing via POST it works as intended`() {

        val result = TestRequest.POST("/test/fake-delete-data-unsafe").execute()

        assertEquals(HttpResponseStatus.OK,result.status)
    }

    @Test
    fun `unsafePOST() fails on GET if token is not provided`() {

        val result = TestRequest.GET("/test/fake-delete-data-unsafe").execute()

        assertEquals(HttpResponseStatus.INTERNAL_SERVER_ERROR,result.status)
    }

    @Test
    fun `ensureSafePOST() fails if token is missing via GET`() {

        val result = TestRequest.GET("/test/fake-delete-data-ensure-safe").execute()

        assertEquals(HttpResponseStatus.INTERNAL_SERVER_ERROR, result.status)
    }

    @Test
    fun `ensureSafePOST() goes wrong on POST if false token is given`() {

        val connection = URL(
            "http://localhost:9999/test/fake-delete-data-ensure-safe?CSRFToken=w-r-o-n-g-t-o-k-e-n"
        ).openConnection() as HttpURLConnection
        connection.setRequestMethod("POST")
        connection.connect()

        assertEquals(401,connection.responseCode )
    }

    @Test
    fun `ensureSafePOST() works as intended if correct token is given via POST`() {

        val connection = URL("http://localhost:9999/test/provide-security-token").openConnection() as HttpURLConnection
        connection.setRequestMethod("GET")
        connection.connect()
        val token = String(Streams.toByteArray(connection.getInputStream()), StandardCharsets.UTF_8)

        val connection2 =
            URL("http://localhost:9999/test/fake-delete-data-ensure-safe?CSRFToken=$token").openConnection() as HttpURLConnection
        connection2.setRequestMethod("POST")
        connection2.setRequestProperty(
            HttpHeaderNames.COOKIE.toString(),
            connection.headerFields["set-cookie"]?.get(0)
        )
        connection2.connect()

        assertEquals(200,connection2.getResponseCode())
    }
}
