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
    fun `a server session round-trips between requests without emitting any cookie`() {

        // The first request writes into the server session...
        val writeConnection = openConnectionWithServerSession("/test/session-test", "roundtrip").apply {
            requestMethod = "GET"
            connect()
        }

        assertEquals(200, writeConnection.responseCode)
        // ...but neither a session cookie nor a session pin cookie is emitted.
        val setCookies = writeConnection.headerFields[HttpHeaderNames.SET_COOKIE.toString()] ?: emptyList()
        assertTrue { setCookies.none { it.startsWith("SIRIUS_SESSION") } }
        assertTrue { setCookies.none { it.contains("PIN", ignoreCase = true) } }
        assertEquals("test", TestServerSessionStorage.getStoredSession("roundtrip")["test1"])

        // The second request reads the value back from the storage (value set to null is not stored).
        val readConnection = openConnectionWithServerSession("/test/session-test-read", "roundtrip").apply {
            requestMethod = "GET"
            connect()
        }

        assertEquals(200, readConnection.responseCode)
        val body = readConnection.inputStream.bufferedReader().readText()
        assertTrue { body.contains("test1=test") }
        assertFalse { body.contains("test2=test") }
    }

    @Test
    fun `a session cookie is ignored when the server session is active`() {

        // Obtain a valid session cookie carrying test1=test the classic way...
        val cookieConnection = openConnectionWithServerSession("/test/session-test").apply {
            requestMethod = "GET"
            connect()
        }
        assertEquals(200, cookieConnection.responseCode)
        val sessionCookie = cookieConnection.headerFields[HttpHeaderNames.SET_COOKIE.toString()]!!
            .first { it.startsWith("SIRIUS_SESSION=") }
            .substringBefore(";")

        // ...and send it along with a server session id: the cookie values must be invisible.
        val readConnection = openConnectionWithServerSession("/test/session-test-read", "cookie-ignored").apply {
            requestMethod = "GET"
            setRequestProperty(HttpHeaderNames.COOKIE.toString(), sessionCookie)
            connect()
        }

        assertEquals(200, readConnection.responseCode)
        assertTrue { readConnection.inputStream.bufferedReader().readText().contains("test1=<none>") }
    }

    @Test
    fun `server session changes on a cacheable response are discarded`() {

        val persistCallsBefore = TestServerSessionStorage.getPersistCalls()

        val connection = openConnectionWithServerSession("/test/session-test-cacheable", "cacheable").apply {
            requestMethod = "GET"
            connect()
        }

        assertEquals(200, connection.responseCode)
        assertEquals(persistCallsBefore, TestServerSessionStorage.getPersistCalls())
        assertFalse { TestServerSessionStorage.hasStoredSession("cacheable") }
    }

    @Test
    fun `a failing server session load yields an empty session and suppresses persisting`() {

        val persistCallsBefore = TestServerSessionStorage.getPersistCalls()

        // The load fails, but the request is still answered normally (fail-open) with an empty session...
        val connection = openConnectionWithServerSession("/test/session-test", TestServerSessionStorage.FAILING_SESSION_ID).apply {
            requestMethod = "GET"
            connect()
        }

        assertEquals(200, connection.responseCode)
        // ...and the written value is NOT persisted, so a transient error cannot wipe the stored session.
        assertEquals(persistCallsBefore, TestServerSessionStorage.getPersistCalls())
        assertFalse { TestServerSessionStorage.hasStoredSession(TestServerSessionStorage.FAILING_SESSION_ID) }
    }

    @Test
    fun `clearing a server session deletes the stored session`() {

        // Seed a stored session...
        val writeConnection = openConnectionWithServerSession("/test/session-test", "to-clear").apply {
            requestMethod = "GET"
            connect()
        }
        assertEquals(200, writeConnection.responseCode)
        assertTrue { TestServerSessionStorage.hasStoredSession("to-clear") }

        // ...and clear it: the storage receives an empty map and removes the session.
        val clearConnection = openConnectionWithServerSession("/test/session-test-clear", "to-clear").apply {
            requestMethod = "GET"
            connect()
        }

        assertEquals(200, clearConnection.responseCode)
        assertFalse { TestServerSessionStorage.hasStoredSession("to-clear") }
    }

    @Test
    fun `a safe GET does not initialize the session just for the CSRF check`() {

        // The CSRF skip condition must be evaluated after the cheap method check: GETs are never CSRF-validated,
        // so they must not pay for the (potentially expensive) session initialization.
        val checksBefore = TestServerSessionStorage.getResponsibilityChecks()

        val connection = openConnectionWithServerSession("/test/redirect-target").apply {
            requestMethod = "GET"
            connect()
        }

        assertEquals(200, connection.responseCode)
        assertEquals(checksBefore, TestServerSessionStorage.getResponsibilityChecks())
    }

    @Test
    fun `a POST without CSRF token is accepted when the server session is active`() {

        // Without the server session header this request yields 403 (see CSRFTokenTest): the CSRF check protects
        // ambient cookie authority. With an active server session the cookie is ignored entirely, so there is
        // nothing to protect and the check is skipped.
        val connection = openConnectionWithServerSession("/test/fake-delete-data", "csrf-skip").apply {
            requestMethod = "POST"
            connect()
        }

        assertEquals(200, connection.responseCode)
    }

    @Test
    fun `a custom uri installed before execute survives the build step`() {

        // build() re-parses the final uri; a uri installed via withCustomURI must remain its base instead of
        // being reset to the constructor uri.
        val request = TestRequest.GET("/test/wrong-route")
        request.withParameters(mapOf("b" to "b"))
        request.withCustomURI("/test/redirect-target?a=a")

        val result = request.execute()

        assertEquals("/test/redirect-target", request.requestedURI)
        assertEquals("a", request.getParameter("a"))
        assertEquals("b", request.getParameter("b"))
        assertEquals(200, result.status.code())
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

    private fun openConnectionWithServerSession(path: String, serverSessionId: String? = null): HttpURLConnection {
        val connection = URI("http://localhost:9999$path").toURL().openConnection() as HttpURLConnection
        serverSessionId?.let { connection.setRequestProperty(TestServerSessionStorage.SESSION_HEADER, it) }
        return connection
    }
}
