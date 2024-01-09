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
import org.junit.jupiter.api.assertDoesNotThrow
import org.junit.jupiter.api.assertThrows
import org.junit.jupiter.api.extension.ExtendWith
import sirius.kernel.SiriusExtension
import sirius.kernel.commons.Json
import sirius.kernel.commons.Streams
import sirius.kernel.commons.Strings
import sirius.kernel.commons.Wait
import sirius.kernel.health.Log
import sirius.kernel.health.LogHelper
import java.io.IOException
import java.net.HttpURLConnection
import java.net.URL
import java.nio.charset.StandardCharsets
import java.util.logging.Level
import kotlin.test.assertEquals

/**
 * Simulates a bunch of "real" (outside) requests through netty and sirius.
 * <p>
 * This ensures a basic performance profile and also makes sure no trivial race conditions or memory leaks
 * are added.
 */
@ExtendWith(SiriusExtension::class)
class WebServerTest {

    private fun callAndRead(
        uri: String,
        outHeaders: Map<String, String?>?,
        expectedHeaders: Map<String, String?>?
    ): String {
        val connection = URL("http://localhost:9999$uri").openConnection() as HttpURLConnection

        outHeaders?.forEach { (k, v) -> connection.addRequestProperty(k, v) }
        connection.connect()
        val result = String(Streams.toByteArray(connection.inputStream), StandardCharsets.UTF_8)
        expectedHeaders?.forEach { k, v ->
            if ("*" == v) {
                if (Strings.isEmpty(connection.getHeaderField(k))) {
                    throw IllegalStateException("Header: $k was expected, but not set")
                }
            } else if (!Strings.areEqual(connection.getHeaderField(k), v)) {
                throw IllegalStateException("Header: " + k + " was " + connection.getHeaderField(k) + " instead of " + v)
            }
        }

        return result
    }

    /**
     * Ensures that re-writing or controller URIs works
     */
    @Test
    fun `Invoke rewritten to check if re-writing works`() {

        val uri = "/rewritten"

        val data = callAndRead(uri, null, null)

        assertEquals("OK", data)
    }

    /**
     * Ensures that set-cookie and caching headers aren't mixed.
     */
    @Test
    fun `Invoke test-cookieCacheTest`() {

        val uri = "/test/cookieCacheTest"
        val headers = mapOf("accept-encoding" to "gzip")
        // File is too small to be compressed!
        val expectedHeaders = mapOf(
            "set-cookie" to "*",
            "expires" to null,
            "cache-control" to "no-cache, max-age=0"
        )
        assertDoesNotThrow { val data = callAndRead(uri, headers, expectedHeaders) }
    }

    @Test
    fun `Invoke assets-test_css to test`() {

        val uri = "/assets/test.css"
        val headers = mapOf("accept-encoding" to "gzip")
        // File is too small to be compressed!
        val expectedHeaders = mapOf("content-encoding" to null)

        val data = callAndRead(uri, headers, expectedHeaders)

        assertEquals("body { background-color: #000000; }", data)
    }

    @Test
    fun `Invoke assets-test_large_css`() {

        val uri = "/assets/test_large.css"
        val expectedHeaders = mapOf("accept-encoding" to null)

        val data = callAndRead(uri, null, expectedHeaders)

        assertEquals(60314, data.length)
    }

    @Test
    fun `Invoke assets-test_large_css with GZIP`() {

        val uri = "/assets/test_large.css"
        val headers = mapOf("accept-encoding" to "gzip")
        val expectedHeaders = mapOf("content-encoding" to "gzip")

        val data = callAndRead(uri, headers, expectedHeaders)

        // URLConnection does not understand GZIP and therefore does not unzip... :-(
        assertEquals(1298, data.length)
    }

    @Test
    fun `Invoke test-resource to access a resource`() {

        val uri = "/test/resource"
        val headers = mapOf("accept-encoding" to "gzip")
        val expectedHeaders = mapOf("content-encoding" to "gzip")

        val data = callAndRead(uri, headers, expectedHeaders)

        // URLConnection does not understand GZIP and therefore does not unzip... :-(
        assertEquals(1298, data.length)
    }

    @Test
    fun `Invoke test-resource_uncompressable to access a non-compressable resource`() {

        val uri = "/test/resource_uncompressable"
        val headers = mapOf("accept-encoding" to "gzip")
        val expectedHeaders = mapOf("content-encoding" to null)

        val data = callAndRead(uri, headers, expectedHeaders)
        assertEquals(60_314, data.length)
    }

    /**
     * Determines if redispatching in the TestDispatcher works.
     */
    @Test
    fun `Redispatching works`() {

        val uri = "/redispatch"
        val expectedHeaders = mapOf("content-type" to "application/json;charset=UTF-8")

        val data = callAndRead(uri, null, expectedHeaders)

        assertEquals("{\"success\":true,\"error\":false,\"test\":true}", data)
    }

    /**
     * Call a small service which result fits into a single response chunk...
     */
    @Test
    fun `Invoke api-test`() {

        val uri = "/api/test"
        val expectedHeaders = mapOf("content-type" to "application/json;charset=UTF-8")

        val data = callAndRead(uri, null, expectedHeaders)

        assertEquals("{\"success\":true,\"error\":false,\"test\":true}", data)
    }

    /**
     * Call a large service to test buffer-based output streams
     */
    @Test
    fun `Invoke api-test-test_large`() {

        val uri = "/api/test/test_large"
        val expectedHeaders = mapOf("content-type" to "application/json;charset=UTF-8")

        val data = callAndRead(uri, null, expectedHeaders)

        // Size should be contents of large test file plus json overhead and escaping....
        assertEquals(60572, data.length)
    }

    /**
     * Call a service which generates a small result and then fails.
     * <p>
     * We expect an appropriate error in this case.
     */
    @Test
    fun `Invoke api-test-test_large_failure and expect a proper error`() {

        val uri = "/api/test/test_large_failure"
        val expectedHeaders = mapOf("content-type" to "text/xml;charset=UTF-8")

        LogHelper.clearMessages()

        // We expect a warning as the server was unable to send an error
        // As the connection is closed due to an inconsistent state, an IO exception will occur on the client side
        assertThrows<IOException> {
            val data = callAndRead(uri, null, expectedHeaders)
            LogHelper.hasMessage(Level.WARNING, Log.get("web"), "Cannot send service error for.*")
        }
    }

    /**
     * Call a controller which tunnels a small file
     */
    @Test
    fun `Invoke tunnel-test`() {

        val uri = "/tunnel/test"
        val expectedHeaders = mapOf("content-type" to "text/test")

        val data = callAndRead(uri, null, expectedHeaders)

        assertEquals("{\"success\":true,\"error\":false,\"test\":true}", data)
    }

    /**
     * Call a controller which tunnels a small file
     */
    @Test
    fun `Invoke tunnel-test-tune with a custom request`() {

        val uri = "/tunnel/test/tune"

        val data = callAndRead(uri, null, null)

        assertEquals("POST", data)
    }

    /**
     * Call a controller which tunnels a large file
     */
    @Test
    fun `Invoke tunnel-test_large`() {

        val uri = "/tunnel/test_large"
        val expectedHeaders = mapOf("content-type" to "application/json;charset=UTF-8")

        val data = callAndRead(uri, null, expectedHeaders)

        // Size should be contents of large test file plus json overhead and escaping....
        assertEquals(60572, data.length)
    }

    /**
     * Calls a controller which transforms a tunnelled file
     * <p>
     * This matches the logic in <tt>TestController.tunnelTestTransform</tt>.
     */
    @Test
    fun `Invoke tunnel-test_transform`() {
        // We load the raw data
        val data = callAndRead("/tunnel/test_large", null, null)
        // We load the transformed data which is byte shifted by +1
        val connection = URL("http://localhost:9999/tunnel/test_transform").openConnection() as HttpURLConnection
        val transformedData = Streams.toByteArray(connection.inputStream)
        // We un-shift all bytes
        val reTransformedData = ByteArray(transformedData.size)

        for (i in transformedData.indices) {
            //reTransformedData[i] = transformedData[i] == 0 ? 255 : transformedData[i] - 1
            //TODO
        }
        // Both should be equivalent in size...
        assertEquals(data.length, transformedData.size)
        // And the re-transformed contents should match...
        assertEquals(data, String(reTransformedData, StandardCharsets.UTF_8))
    }

    /**
     * Call a controller which uses a fallback for a failed tunnel (404)
     */
    @Test
    fun `Invoke tunnel-fallback_for_404 and expect the fallback to work`() {

        val uri = "/tunnel/fallback_for_404"
        val expectedHeaders = mapOf("content-type" to "text/test")

        val data = callAndRead(uri, null, expectedHeaders)

        assertEquals("{\"success\":true,\"error\":false,\"test\":true}", data)
    }

    /**
     * Call a controller which uses a fallback for a failed tunnel (connection error)
     */
    @Test
    fun `Invoke tunnel-fallback_for_error and expect the fallback to work`() {

        val uri = "/tunnel/fallback_for_error"
        val expectedHeaders = mapOf("content-type" to "text/test")

        val data = callAndRead(uri, null, expectedHeaders)

        assertEquals("{\"success\":true,\"error\":false,\"test\":true}", data)
    }

    /**
     * Call a controller which uses JSON Calls
     */
    @Test
    fun `Invoke test-json testing built in JSON handling`() {

        val uri = "/test/json?test=Hello_World"
        val expectedHeaders = mapOf("content-type" to "application/json;charset=UTF-8")

        val data = callAndRead(uri, null, expectedHeaders)

        assertEquals("Hello_World", Json.parseObject(data).get("test").asText())
    }

    @Test
    fun `Invoke test-json-param testing built in JSON handling`() {

        val uri = "/test/json-param/Hello"
        val expectedHeaders = mapOf("content-type" to "application/json;charset=UTF-8")

        val data = callAndRead(uri, null, expectedHeaders)

        assertEquals("Hello", Json.parseObject(data).get("test").asText())
    }

    @Test
    fun `Invoke test-json-params-1-2 testing multiple parameter`() {

        val uri = "/test/json-params/1/2"
        val expectedHeaders = mapOf("content-type" to "application/json;charset=UTF-8")

        val data = callAndRead(uri, null, expectedHeaders)

        assertEquals("1", Json.parseObject(data).get("param1").asText())
        assertEquals("2", Json.parseObject(data).get("param2").asText())
    }

    @Test
    fun `Invoke test-mixed-json-params-2-1 testing mixed parameter order`() {

        val uri = "/test/mixed-json-params/2/1"
        val expectedHeaders = mapOf("content-type" to "application/json;charset=UTF-8")

        val data = callAndRead(uri, null, expectedHeaders)

        assertEquals("1", Json.parseObject(data).get("param1").asText())
        assertEquals("2", Json.parseObject(data).get("param2").asText())
    }

    @Test
    fun `Invoke test-json-params-varargs-1-2-3-4-5-6-7-8-9 testing varargs`() {

        val uri = "/test/json-params-varargs/1/2/3/4/5/6/7/8/9"
        val expectedHeaders = mapOf("content-type" to "application/json;charset=UTF-8")

        val data = callAndRead(uri, null, expectedHeaders)

        assertEquals("1", Json.parseObject(data).get("param1").asText())
        assertEquals("2", Json.parseObject(data).get("param2").asText())

        val varargs = Json.getArray(Json.parseObject(data), "params")
        assertEquals(7, varargs.size())
        assertEquals("3", varargs.get(0).asText())
        assertEquals("9", varargs.get(6).asText())
    }

    /**
     * Call a controller which uses predispatching
     * <p>
     * Also expects that the controller support keepalive after a successful request/response.
     */
    @Test
    fun `Invoke test-predispatch with POST`() {

        val url = URL("http://localhost:9999/test/predispatch").openConnection() as HttpURLConnection

        val testByteArray = "Hello Service".toByteArray()

        url.setRequestMethod("POST")
        url.setDoInput(true)
        url.setDoOutput(true)
        val out = url.outputStream
        for (i in 1..1024) {
            out.write(testByteArray)
        }
        out.close()
        val result = String(Streams.toByteArray(url.inputStream), StandardCharsets.UTF_8)

        assertEquals(result, (testByteArray.size * 1024).toString())
        assertEquals(HttpHeaderNames.KEEP_ALIVE.toString(), url.getHeaderField(HttpHeaderNames.CONNECTION.toString()))
    }

    /**
     * Call a controller which uses predispatching but responds before the content has been read.
     *
     */
    @Test
    fun `test-predispatch-abort discards an upload and then generates an error as response`() {

        val url = URL("http://localhost:9999/test/predispatch/abort").openConnection() as HttpURLConnection

        url.setChunkedStreamingMode(1024)

        val testByteArray = "X".toByteArray()

        url.setRequestMethod("POST")
        url.setDoInput(true)
        url.setDoOutput(true)

        // Write some data and flush so that the server triggers a response
        val out = url.outputStream
        for (i in 0..1024) {
            out.write(testByteArray)
        }
        out.flush()

        // Slow down to ensure that the response is created and sent
        // Still no IOException is expected, as the server will discard all data..
        Wait.millis(200)
        for (i in 0..1024) {
            out.write(testByteArray)
        }

        val result = String(Streams.toByteArray(url.inputStream), StandardCharsets.UTF_8)

        // We still expect a proper response
        assertEquals("ABORT", result)
    }

    /**
     * Call a controller which uses POST
     */
    @Test
    fun `Invoke test-post with POST`() {

        val url = URL("http://localhost:9999/test/post").openConnection() as HttpURLConnection

        val testString = "value=Hello"

        url.setRequestMethod("POST")
        url.setRequestProperty(
            "Content-Type",
            "application/x-www-form-urlencoded"
        )

        url.setRequestProperty("Content-Length", testString.toByteArray().size.toString())
        url.setDoInput(true)
        url.setDoOutput(true)
        val out = url.outputStream
        out.write(testString.toByteArray(StandardCharsets.UTF_8))
        out.close()
        val result = String(Streams.toByteArray(url.inputStream), StandardCharsets.UTF_8)

        assertEquals("Hello", result)
    }

    @Test
    fun `test that outputstreams work`() {

        val url = URL("http://localhost:9999/test/os").openConnection() as HttpURLConnection

        url.setRequestMethod("GET")
        url.setDoInput(true)
        url.setDoOutput(false)
        val arr = Streams.toByteArray(url.inputStream)

        assertEquals(9 * 8192, arr.size)
    }

    /**
     * Test an empty POST
     */
    @Test
    fun `Invoke test-post with empty POST`() {

        val url = URL("http://localhost:9999/test/post").openConnection() as HttpURLConnection

        val testString = ""

        url.setRequestMethod("POST")
        url.setRequestProperty(
            "Content-Type",
            "application/x-www-form-urlencoded"
        )

        url.setRequestProperty("Content-Length", testString.toByteArray().size.toString())
        url.setDoInput(true)
        url.setDoOutput(true)
        val out = url.outputStream
        out.write(testString.toByteArray(StandardCharsets.UTF_8))
        out.close()
        val result = String(Streams.toByteArray(url.inputStream), StandardCharsets.UTF_8)

        assertEquals("", result)
    }

    /**
     * Ensure that predispatching does not trigger on GET requests
     */
    @Test
    fun `Invoke test-presidpatch with GET`() {

        val url = URL("http://localhost:9999/test/predispatch").openConnection() as HttpURLConnection

        url.setRequestMethod("GET")

        assertEquals(404, url.responseCode)
    }

    /**
     * Test correct decoding
     */
    @Test
    fun `Invoke test-json testing correct decoding delimiter`() {

        val uri = "/test/json?test=Hello%2FWorld"
        val expectedHeaders = mapOf("content-type" to "application/json;charset=UTF-8")

        val data = callAndRead(uri, null, expectedHeaders)

        assertEquals("Hello/World", Json.parseObject(data).get("test").asText())
    }

    @Test
    fun `Invoke test-json testing correct decoding space`() {

        val uri = "/test/json?test=Hello%20World"
        val expectedHeaders = mapOf("content-type" to "application/json;charset=UTF-8")

        val data = callAndRead(uri, null, expectedHeaders)

        assertEquals("Hello World", Json.parseObject(data).get("test").asText())
    }

    @Test
    fun `Invoke test-json-param testing correct decoding delimiter`() {

        val uri = "/test/json-param/Hello%2FWorld"
        val expectedHeaders = mapOf("content-type" to "application/json;charset=UTF-8")

        val data = callAndRead(uri, null, expectedHeaders)

        assertEquals("Hello/World", Json.parseObject(data).get("test").asText())
    }

    @Test
    fun `Invoke test-json-param testing correct decoding space`() {

        val uri = "/test/json-param/Hello%20World"
        val expectedHeaders = mapOf("content-type" to "application/json;charset=UTF-8")

        val data = callAndRead(uri, null, expectedHeaders)

        assertEquals("Hello World", Json.parseObject(data).get("test").asText())
    }

    @Test
    fun `Invoke test-json-params-one-t%2Fwotesting multiple parameter decoding delimiter`() {

        val uri = "/test/json-params/one/t%2Fwo"
        val expectedHeaders = mapOf("content-type" to "application/json;charset=UTF-8")

        val data = callAndRead(uri, null, expectedHeaders)

        assertEquals("one", Json.parseObject(data).get("param1").asText())
        assertEquals("t/wo", Json.parseObject(data).get("param2").asText())
    }

    @Test
    fun `Invoke test-json-params-one-t%20wo testing multiple parameter decoding space`() {

        val uri = "/test/json-params/one/t%20wo"
        val expectedHeaders = mapOf("content-type" to "application/json;charset=UTF-8")

        val data = callAndRead(uri, null, expectedHeaders)

        assertEquals("one", Json.parseObject(data).get("param1").asText())
        assertEquals("t wo", Json.parseObject(data).get("param2").asText())
    }

    @Test
    fun `Invoke test-json-params-varargs-1%2F-%2F2-one-t%2Fwo-t%2Fhree-%2Ffour-five%2F testing varargs decoding delimiter`() {

        val uri = "/test/json-params-varargs/1%2F/%2F2/one/t%2Fwo/t%2Fhree/%2Ffour/five%2F"
        val expectedHeaders = mapOf("content-type" to "application/json;charset=UTF-8")

        val data = callAndRead(uri, null, expectedHeaders)

        assertEquals("1/", Json.parseObject(data).get("param1").asText())
        assertEquals("/2", Json.parseObject(data).get("param2").asText())

        val varargs = Json.getArray(Json.parseObject(data), "params")

        assertEquals(5, varargs.size())
        assertEquals("one", varargs.get(0).asText())
        assertEquals("t/wo", varargs.get(1).asText())
        assertEquals("t/hree", varargs.get(2).asText())
        assertEquals("/four", varargs.get(3).asText())
        assertEquals("five/", varargs.get(4).asText())
    }

    @Test
    fun `Invoke test-json-params-varargs-1%20-%202-one-t%20wo-t%20hree-%20four-five%20 testing varargs decoding space`() {

        val uri = "/test/json-params-varargs/1%20/%202/one/t%20wo/t%20hree/%20four/five%20"
        val expectedHeaders = mapOf("content-type" to "application/json;charset=UTF-8")

        val data = callAndRead(uri, null, expectedHeaders)

        assertEquals("1 ", Json.parseObject(data).get("param1").asText())
        assertEquals(" 2", Json.parseObject(data).get("param2").asText())

        val varargs = Json.getArray(Json.parseObject(data), "params")

        assertEquals(5, varargs.size())
        assertEquals("one", varargs.get(0).asText())
        assertEquals("t wo", varargs.get(1).asText())
        assertEquals("t hree", varargs.get(2).asText())
        assertEquals(" four", varargs.get(3).asText())
        assertEquals("five ", varargs.get(4).asText())
    }

    @Test
    fun `Invoke test-json-param testing param with only delimiter`() {

        val uri = "/test/json-param/%2F%2F%2F"
        val expectedHeaders = mapOf("content-type" to "application/json;charset=UTF-8")

        val data = callAndRead(uri, null, expectedHeaders)

        assertEquals("///", Json.parseObject(data).get("test").asText())
    }

    @Test
    fun `Invoke test-json-param testing param with only space`() {

        val uri = "/test/json-param/%20%20%20"
        val expectedHeaders = mapOf("content-type" to "application/json;charset=UTF-8")

        val data = callAndRead(uri, null, expectedHeaders)

        assertEquals("   ", Json.parseObject(data).get("test").asText())
    }

    @Test
    fun `testRequest follows redirects if instructed`() {

        val response1 = TestRequest.GET("/test/redirect-to-get").execute()
        val response2 = TestRequest.GET("/test/redirect-to-get").followRedirect().execute()

        assertEquals(TestResponse.ResponseType.TEMPORARY_REDIRECT, response1.type)
        assertEquals(HttpResponseStatus.FOUND, response1.status)

        assertEquals(TestResponse.ResponseType.DIRECT, response2.type)
        assertEquals(HttpResponseStatus.OK, response2.status)
    }
}
