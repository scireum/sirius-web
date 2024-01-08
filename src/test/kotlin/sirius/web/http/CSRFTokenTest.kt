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

@ExtendWith(SiriusExtension::class)
class CSRFTokenTest {

    @Test
    fun `safePOST() works correctly if token is missing via GET`() {

        val result = TestRequest.GET("/test/fake-delete-data").execute()

        result.getStatus() == HttpResponseStatus.INTERNAL_SERVER_ERROR
    }

    @Test
    fun `safePOST() works correctly if token is present via GET`() {

        val c = URL("http://localhost:9999/test/provide-security-token").openConnection() as HttpURLConnection
        c.setRequestMethod("GET")
        c.connect()
        val token = String(Streams.toByteArray(c.getInputStream()), StandardCharsets.UTF_8)

        val result = TestRequest.GET("/test/fake-delete-data?CSRFToken=" + token).execute()

        result.getStatus() == HttpResponseStatus.INTERNAL_SERVER_ERROR
    }

    @Test
    fun `safePOST() works correctly if token is missing via POST`() {

        val result = TestRequest.POST("/test/fake-delete-data").execute()

        result.getStatus() == HttpResponseStatus.INTERNAL_SERVER_ERROR
    }

    @Test
    fun `safePOST() works correctly if correct token is present via SAFEPOST`() {

        val result = TestRequest.SAFEPOST("/test/fake-delete-data").execute()

        result.getStatus() == HttpResponseStatus.OK
    }

    @Test
    fun `safePOST() works correctly if correct token is present via POST`() {

        val c = URL("http://localhost:9999/test/provide-security-token").openConnection() as HttpURLConnection
        c.setRequestMethod("GET")
        c.connect()
        val token = String(Streams.toByteArray(c.getInputStream()), StandardCharsets.UTF_8)


        val c2 = URL(
            "http://localhost:9999/test/fake-delete-data?CSRFToken=" + token
        ).openConnection() as HttpURLConnection
        c2.setRequestMethod("POST")
        c2.setRequestProperty(HttpHeaderNames.COOKIE.toString(), c.getHeaderFields().get("set-cookie")?.get(0))
        c2.connect()

        c2.getResponseCode() == 200
    }

    @Test
    fun `safePOST() works correctly if expired token is present via POST`() {

        val c = URL("http://localhost:9999/test/provide-security-token").openConnection() as HttpURLConnection
        c.setRequestMethod("GET")
        c.connect()
        val token = String(Streams.toByteArray(c.getInputStream()), StandardCharsets.UTF_8)
        TestRequest.GET("/test/expire-security-token").execute()


        val c2 = URL(
            "http://localhost:9999/test/fake-delete-data?CSRFToken=" + token
        ).openConnection() as HttpURLConnection
        c2.setRequestMethod("POST")
        c2.setRequestProperty(HttpHeaderNames.COOKIE.toString(), c.getHeaderFields().get("set-cookie")?.get(0))
        c2.connect()

        c2.getResponseCode() == 200
    }

    @Test
    fun `safePOST() works correctly if wrong token is present via POST`() {

        val c = URL("http://localhost:9999/test/provide-security-token").openConnection() as HttpURLConnection
        c.setRequestMethod("GET")
        c.connect()
        val token = String(Streams.toByteArray(c.getInputStream()), StandardCharsets.UTF_8)


        val c2 =
            URL("http://localhost:9999/test/fake-delete-data?CSRFToken=w-r-o-n-g-t-o-k-e-n").openConnection() as HttpURLConnection
        c2.setRequestMethod("POST")
        c2.setRequestProperty(HttpHeaderNames.COOKIE.toString(), c.getHeaderFields().get("set-cookie")?.get(0))
        c2.connect()

        c2.getResponseCode() == 500
    }

    @Test
    fun `unsafePOST() works correctly if token is missing via POST`() {

        val result = TestRequest.POST("/test/fake-delete-data-unsafe").execute()

        result.getStatus() == HttpResponseStatus.OK
    }

    @Test
    fun `unsafePOST() works correctly if token is missing via GET`() {

        val result = TestRequest.GET("/test/fake-delete-data-unsafe").execute()

        result.getStatus() == HttpResponseStatus.INTERNAL_SERVER_ERROR
    }

    @Test
    fun `ensureSafePOST() works correctly if token is missing via GET`() {

        val result = TestRequest.GET("/test/fake-delete-data-ensure-safe").execute()

        result.getStatus() == HttpResponseStatus.INTERNAL_SERVER_ERROR
    }

    @Test
    fun `ensureSafePOST() works correctly if wrong token is present via POST`() {

        val c = URL(
            "http://localhost:9999/test/fake-delete-data-ensure-safe?CSRFToken=w-r-o-n-g-t-o-k-e-n"
        ).openConnection() as HttpURLConnection
        c.setRequestMethod("POST")
        c.connect()

        c.getResponseCode() == 401
    }

    @Test
    fun `ensureSafePOST() works correctly if correct token is present via POST`() {

        val c = URL("http://localhost:9999/test/provide-security-token").openConnection() as HttpURLConnection
        c.setRequestMethod("GET")
        c.connect()
        val token = String(Streams.toByteArray(c.getInputStream()), StandardCharsets.UTF_8)


        val c2 =
            URL("http://localhost:9999/test/fake-delete-data-ensure-safe?CSRFToken=" + token).openConnection() as HttpURLConnection
        c2.setRequestMethod("POST")
        c2.setRequestProperty(HttpHeaderNames.COOKIE.toString(), c.getHeaderFields().get("set-cookie")?.get(0))
        c2.connect()

        c2.getResponseCode() == 200
    }
}
