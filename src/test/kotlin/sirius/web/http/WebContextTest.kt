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
import org.junit.jupiter.params.provider.ValueSource
import org.junit.runner.RunWith
import sirius.kernel.SiriusExtension
import java.net.HttpURLConnection
import java.net.URI
import kotlin.test.*

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


    @ParameterizedTest
    fun `parseAcceptLanguage works as expected`(header: String, lang: String) {
        assertEquals(
            lang, TestRequest.GET("/test?a=a").addHeader(HttpHeaderNames.ACCEPT_LANGUAGE, header).fetchLanguage()
                .orElse(null)
        )
        assertContains(lang,header)
        assertContains("de","de, en;q=0.8")
        assertContains("en","en, de;q=0.8")
        assertContains("de","xx, de;q=0.8, en-gb;q=0.7")
        assertContains("en","xx, de;q=0.5, en-gb;q=0.7")
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

        val c = URI("http://localhost:9999/test/session-test").toURL().openConnection() as HttpURLConnection
        c.setRequestMethod("GET")
        c.connect()

        assertEquals(200, c.responseCode)
        assertTrue { c.headerFields[HttpHeaderNames.SET_COOKIE.toString()]!![0].contains("test1=test") }
        assertFalse { c.headerFields[HttpHeaderNames.SET_COOKIE.toString()]!![0].contains("test2=") }

    }
}
