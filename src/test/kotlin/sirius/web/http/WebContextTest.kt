/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.http

import io.netty.handler.codec.http.HttpHeaderNames
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.extension.ExtendWith
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.CsvSource
import sirius.kernel.SiriusExtension
import java.net.HttpURLConnection
import java.net.URI
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertTrue

@ExtendWith(SiriusExtension::class)
class WebContextTest {

    @Test
    fun `getQueryString returns the full query string`() {

        val request = TestRequest.GET("/test?a=a&b=b")

        assertEquals("a", request.getParameter("a"))
        assertEquals("b", request.getParameter("b"))
        assertEquals("a=a&b=b", request.queryString)
    }

    @Test
    fun `getQueryString returns an empty string when no query string is present`() {

        val request = TestRequest.GET("/test")

        assertEquals("", request.queryString)
    }

    @Test
    fun `getQueryString returns an empty string when an empty query string is present`() {

        val request = TestRequest.GET("/test?")

        assertEquals("", request.queryString)
    }

    @Test
    fun `withCustomURI rewrites the uri correctly and removes the existing query string`() {

        val request = TestRequest.GET("/test?a=a")

        request.withCustomURI("/test%2Ftest?b=b")

        assertEquals("/test%2Ftest", request.rawRequestedURI)
        assertEquals("/test/test", request.requestedURI)
        assertFalse { request.get("a").isFilled }
        assertTrue { request.get("b").isFilled }
    }

    @Test
    fun `withCustomPath rewrites the path correctly without removing the existing query string`() {

        val request = TestRequest.GET("/test?a=a")

        request.withCustomPath("/test/test")
        assertEquals("/test/test", request.rawRequestedURI)
        assertEquals("/test/test", request.requestedURI)
        assertTrue { request.get("a").isFilled }
    }

    @CsvSource(
        delimiter = '|', useHeadersInDisplayName = true, textBlock = // language=CSV
            """header                      | lang
            de, en;q=0.8                | de
            en, de;q=0.8                | en
            xx, de;q=0.8, en-gb;q=0.7   | de
            xx, de;q=0.5, en-gb;q=0.7   | en"""
    )
    @ParameterizedTest
    fun `parseAcceptLanguage works as expected`(header: String, lang: String) {
        assertEquals(
            lang, TestRequest.GET("/test?a=a").addHeader(HttpHeaderNames.ACCEPT_LANGUAGE, header).fetchLanguage()
                .orElse(null)
        )
    }

    @Test
    fun `getCompletionPromise() works if a promise has been installed`() {
        CompletionPromiseTestController.lastPromisedReturnCode = 0

        val connection =
            URI("http://localhost:9999/test/completion-promise").toURL().openConnection() as HttpURLConnection
        connection.setRequestMethod("GET")
        connection.connect()
        synchronized(CompletionPromiseTestController.SIGNAL) {
            CompletionPromiseTestController.SIGNAL.let { Thread.sleep(1000) }
        }

        assertEquals(200, connection.responseCode)
        assertEquals(200, CompletionPromiseTestController.lastPromisedReturnCode)
    }

    @Test
    fun `getCompletionPromise() works if invoked after completion`() {

        val request = TestRequest.GET("/test?a=a")

        val result = request.execute()

        assertTrue { request.getCompletionPromise().isSuccessful }

    }

    @Test
    fun `setSessionValue works as expected`() {

        // The first request stores the session values and returns the (encrypted) session cookie.
        val writeConnection =
            URI("http://localhost:9999/test/session-test").toURL().openConnection() as HttpURLConnection
        writeConnection.requestMethod = "GET"
        writeConnection.connect()

        assertEquals(200, writeConnection.responseCode)
        val sessionCookie = writeConnection.headerFields[HttpHeaderNames.SET_COOKIE.toString()]!!
            .first { it.startsWith("SIRIUS_SESSION=") }
            .substringBefore(";")

        // The second request reads the session back, proving that the value round-trips through the encrypted
        // cookie and that a value set to null is not stored.
        val readConnection =
            URI("http://localhost:9999/test/session-test-read").toURL().openConnection() as HttpURLConnection
        readConnection.requestMethod = "GET"
        readConnection.setRequestProperty(HttpHeaderNames.COOKIE.toString(), sessionCookie)
        readConnection.connect()

        assertEquals(200, readConnection.responseCode)
        val body = readConnection.inputStream.bufferedReader().readText()
        assertTrue { body.contains("test1=test") }
        assertFalse { body.contains("test2=test") }

    }

    @Test
    fun `the session cookie keeps its attributes and is not partitioned by default`() {

        // Guards the setSessionScopedCookie refactor: the session cookie must still be marked HttpOnly (the attribute
        // moved into the new helper), and - since http.sessionCookie.partitioned defaults to false - must NOT carry the
        // Partitioned (CHIPS) attribute, so behaviour is unchanged for existing products.
        val connection =
            URI("http://localhost:9999/test/session-test").toURL().openConnection() as HttpURLConnection
        connection.requestMethod = "GET"
        connection.connect()

        assertEquals(200, connection.responseCode)
        val sessionCookieLine = connection.headerFields[HttpHeaderNames.SET_COOKIE.toString()]!!
            .first { it.startsWith("SIRIUS_SESSION=") }

        assertTrue { sessionCookieLine.contains("HTTPOnly", ignoreCase = true) }
        assertFalse { sessionCookieLine.contains("Partitioned", ignoreCase = true) }

    }

    @Test
    fun `a legacy unencrypted session cookie is read and upgraded to the encrypted format`() {

        // Build a legacy (unencrypted) session cookie in the "<sha512 hash>:<querystring>" format, signed with the
        // test secret "TEST" (see component-test-web.conf).
        val value = "?test1=test"
        val hash = java.security.MessageDigest.getInstance("SHA-512")
            .digest((value + "TEST").toByteArray())
            .joinToString("") { "%02x".format(it) }
        val legacyCookie = "SIRIUS_SESSION=$hash:$value"

        val connection =
            URI("http://localhost:9999/test/session-test-read").toURL().openConnection() as HttpURLConnection
        connection.requestMethod = "GET"
        connection.setRequestProperty(HttpHeaderNames.COOKIE.toString(), legacyCookie)
        connection.connect()

        assertEquals(200, connection.responseCode)
        // The legacy cookie is decoded correctly...
        assertTrue { connection.inputStream.bufferedReader().readText().contains("test1=test") }
        // ...and eagerly re-written in the encrypted format (marked with the "E1:" prefix).
        val rewrittenCookie = connection.headerFields[HttpHeaderNames.SET_COOKIE.toString()]!!
            .first { it.startsWith("SIRIUS_SESSION=") }
        assertTrue { rewrittenCookie.contains("SIRIUS_SESSION=E1:") }
        assertFalse { rewrittenCookie.contains("test1=test") }

    }
}
