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
import sirius.kernel.BaseSpecification
import sirius.kernel.commons.Json
import sirius.kernel.commons.Streams
import sirius.kernel.commons.Strings
import sirius.kernel.commons.Wait
import sirius.kernel.health.LogHelper

import java.nio.charset.StandardCharsets
import java.util.logging.Level

/**
 * Simulates a bunch of "real" (outside) requests through netty and sirius.
 * <p>
 * This ensures a basic performance profile and also makes sure no trivial race conditions or memory leaks
 * are added.
 */
class WebServerSpec extends BaseSpecification {

    static String callAndRead(String uri, Map outHeaders, Map expectedHeaders) {
        URLConnection c = new URL("http://localhost:9999" + uri).openConnection()
        outHeaders.each { k, v -> c.addRequestProperty(k, v) }
        c.connect()
        def result = new String(Streams.toByteArray(c.getInputStream()), StandardCharsets.UTF_8)
        expectedHeaders.each { k, v ->
            if ("*" == v) {
                if (Strings.isEmpty(c.getHeaderField(k))) {
                    throw new IllegalStateException("Header: " + k + " was expected, but not set")
                }
            } else if (!Strings.areEqual(c.getHeaderField(k), v)) {
                throw new IllegalStateException("Header: " + k + " was " + c.getHeaderField(k) + " instead of " + v)
            }
        }

        return result
    }

    /**
     * Ensures that re-writing or controller URIs works
     */
    def "Invoke /rewritten to check if re-writing works"() {
        given:
        def uri = "/rewritten"
        when:
        def data = callAndRead(uri, null, null)
        then:
        data == "OK"
    }

    /**
     * Ensures that set-cookie and caching headers aren't mixed.
     */
    def "Invoke /test/cookieCacheTest"() {
        given:
        def uri = "/test/cookieCacheTest"
        def headers = ['accept-encoding': 'gzip']
        // File is too small to be compressed!
        def expectedHeaders = ['set-cookie': '*', 'expires': null, 'cache-control': 'no-cache, max-age=0']
        when:
        def data = callAndRead(uri, headers, expectedHeaders)
        then:
        notThrown(IllegalStateException)
    }

    def "Invoke /assets/test.css to test"() {
        given:
        def uri = "/assets/test.css"
        def headers = ['accept-encoding': 'gzip']
        // File is too small to be compressed!
        def expectedHeaders = ['content-encoding': null]
        when:
        def data = callAndRead(uri, headers, expectedHeaders)
        then:
        "body { background-color: #000000; }" == data
    }

    def "Invoke /assets/test_large.css"() {
        given:
        def uri = "/assets/test_large.css"
        def expectedHeaders = ['content-encoding': null]
        when:
        def data = callAndRead(uri, null, expectedHeaders)
        then:
        60314 == data.length()
    }

    def "Invoke /assets/test_large.css with GZIP"() {
        given:
        def uri = "/assets/test_large.css"
        def headers = ['accept-encoding': 'gzip']
        def expectedHeaders = ['content-encoding': 'gzip']
        when:
        def data = callAndRead(uri, headers, expectedHeaders)
        then:
        // URLConnection does not understand GZIP and therefore does not unzip... :-(
        1298 == data.length()
    }

    def "Invoke /test/resource to access a resource"() {
        given:
        def uri = "/test/resource"
        def headers = ['accept-encoding': 'gzip']
        def expectedHeaders = ['content-encoding': 'gzip']
        when:
        def data = callAndRead(uri, headers, expectedHeaders)
        then:
        // URLConnection does not understand GZIP and therefore does not unzip... :-(
        1298 == data.length()
    }

    def "Invoke /test/resource_uncompressable to access a non-compressable resource"() {
        given:
        def uri = "/test/resource_uncompressable"
        def headers = ['accept-encoding': 'gzip']
        def expectedHeaders = ['content-encoding': null]
        when:
        def data = callAndRead(uri, headers, expectedHeaders)
        then:
        60_314 == data.length()
    }

    /**
     * Determines if redispatching in the TestDispatcher works.
     */
    def "Redispatching works"() {
        given:
        def uri = "/redispatch"
        def expectedHeaders = ['content-type': 'application/json;charset=UTF-8']
        when:
        def data = callAndRead(uri, null, expectedHeaders)
        then:
        '{"success":true,"error":false,"test":true}' == data
    }

    /**
     * Call a small service which result fits into a single response chunk...
     */
    def "Invoke /api/test"() {
        given:
        def uri = "/api/test"
        def expectedHeaders = ['content-type': 'application/json;charset=UTF-8']
        when:
        def data = callAndRead(uri, null, expectedHeaders)
        then:
        '{"success":true,"error":false,"test":true}' == data
    }

    /**
     * Call a large service to test buffer-based output streams
     */
    def "Invoke /api/test/test_large"() {
        given:
        def uri = "/api/test/test_large"
        def expectedHeaders = ['content-type': 'application/json;charset=UTF-8']
        when:
        def data = callAndRead(uri, null, expectedHeaders)
        then:
        // Size should be contents of large test file plus json overhead and escaping....
        60572 == data.length()
    }

    /**
     * Call a service which generates a small result and then fails.
     * <p>
     * We expect an appropriate error in this case.
     */
    def "Invoke /api/test/test_large_failure and expect a proper error"() {
        given:
        def uri = "/api/test/test_large_failure"
        def expectedHeaders = ['content-type': 'text/xml;charset=UTF-8']
        when:
        LogHelper.INSTANCE.clearMessages()
        and:
        def data = callAndRead(uri, null, expectedHeaders)
        then: "We expect a warning as the server was unable to send an error"
        LogHelper.INSTANCE.hasMessage(Level.WARNING, "web", "Cannot send service error for.*")
        and: "As the connection is closed due to an inconsistent state, an IO exception will occur on the client side"
        thrown(IOException)
    }

    /**
     * Call a controller which tunnels a small file
     */
    def "Invoke /tunnel/test"() {
        given:
        def uri = "/tunnel/test"
        def expectedHeaders = ['content-type': 'text/test']
        when:
        def data = callAndRead(uri, null, expectedHeaders)
        then:
        '{"success":true,"error":false,"test":true}' == data
    }

    /**
     * Call a controller which tunnels a small file
     */
    def "Invoke /tunnel/test/tune with a custom request"() {
        given:
        def uri = "/tunnel/test/tune"
        when:
        def data = callAndRead(uri, null, null)
        then:
        'POST' == data
    }

    /**
     * Call a controller which tunnels a large file
     */
    def "Invoke /tunnel/test_large"() {
        given:
        def uri = "/tunnel/test_large"
        def expectedHeaders = ['content-type': 'application/json;charset=UTF-8']
        when:
        def data = callAndRead(uri, null, expectedHeaders)
        then:
        // Size should be contents of large test file plus json overhead and escaping....
        60572 == data.length()
    }

    /**
     * Calls a controller which transforms a tunnelled file
     * <p>
     * This matches the logic in <tt>TestController.tunnelTestTransform</tt>.
     */
    def "Invoke /tunnel/test_transform"() {
        when: "we load the raw data"
        def data = callAndRead("/tunnel/test_large", null, null)
        and: "we load the transformed data which is byte shifted by +1"
        HttpURLConnection c = new URL("http://localhost:9999/tunnel/test_transform").openConnection()
        def transformedData = Streams.toByteArray(c.getInputStream())
        and: "we un-shift all bytes"
        def reTransformedData = new byte[transformedData.length]
        for (int i = 0; i < transformedData.length; i++) {
            reTransformedData[i] = transformedData[i] == 0 ? 255 : transformedData[i] - 1
        }
        then: "Both should be equivalent in size..."
        transformedData.length == data.length()
        and: "And the re-transformed contents should match..."
        new String(reTransformedData, StandardCharsets.UTF_8) == data
    }

    /**
     * Call a controller which uses a fallback for a failed tunnel (404)
     */
    def "Invoke /tunnel/fallback_for_404 and expect the fallback to work"() {
        given:
        def uri = "/tunnel/fallback_for_404"
        def expectedHeaders = ['content-type': 'text/test']
        when:
        def data = callAndRead(uri, null, expectedHeaders)
        then:
        '{"success":true,"error":false,"test":true}' == data
    }

    /**
     * Call a controller which uses a fallback for a failed tunnel (connection error)
     */
    def "Invoke /tunnel/fallback_for_error and expect the fallback to work"() {
        given:
        def uri = "/tunnel/fallback_for_error"
        def expectedHeaders = ['content-type': 'text/test']
        when:
        def data = callAndRead(uri, null, expectedHeaders)
        then:
        '{"success":true,"error":false,"test":true}' == data
    }

    /**
     * Call a controller which uses predispatching
     * <p>
     * Also expects that the controller support keepalive after a successful request/response.
     */
    def "Invoke /test/predispatch with POST"() {
        given:
        HttpURLConnection u = new URL("http://localhost:9999/test/predispatch").openConnection()
        and:
        def testByteArray = "Hello Service".getBytes()
        when:
        u.setRequestMethod("POST")
        u.setDoInput(true)
        u.setDoOutput(true)
        def out = u.getOutputStream()
        for (int i = 0; i < 1024; i++) {
            out.write(testByteArray)
        }
        out.close()
        def result = new String(Streams.toByteArray(u.getInputStream()), StandardCharsets.UTF_8)
        then:
        String.valueOf(testByteArray.length * 1024) == result
        and:
        u.getHeaderField(HttpHeaderNames.CONNECTION.toString()) == HttpHeaderNames.KEEP_ALIVE.toString()
    }

    /**
     * Call a controller which uses predispatching but responds before the content has been read.
     *
     */
    def "/test/predispatch/abort discards an upload and then generates an error as response"() {
        given:
        HttpURLConnection u = new URL("http://localhost:9999/test/predispatch/abort").openConnection()
        and:
        u.setChunkedStreamingMode(1024)
        and:
        def testByteArray = "X".getBytes()
        when:
        u.setRequestMethod("POST")
        u.setDoInput(true)
        u.setDoOutput(true)

        // Write some data and flush so that the server triggers a response
        def out = u.getOutputStream()
        for (int i = 0; i < 1024; i++) {
            out.write(testByteArray)
        }
        out.flush()

        // Slow down to ensure that the response is created and sent
        // Still no IOException is expected, as the server will discard all data..
        Wait.millis(200)
        for (int i = 0; i < 1024; i++) {
            out.write(testByteArray)
        }

        def result = new String(Streams.toByteArray(u.getInputStream()), StandardCharsets.UTF_8)
        then:
        // We still expect a proper response
        "ABORT" == result
    }

    /**
     * Call a controller which uses POST
     */
    def "Invoke /test/post with POST"() {
        given:
        HttpURLConnection u = new URL("http://localhost:9999/test/post").openConnection()
        and:
        def testString = "value=Hello"
        when:
        u.setRequestMethod("POST")
        u.setRequestProperty("Content-Type",
                             "application/x-www-form-urlencoded")

        u.setRequestProperty("Content-Length", Integer.toString(testString.getBytes().length))
        u.setDoInput(true)
        u.setDoOutput(true)
        def out = u.getOutputStream()
        out.write(testString.getBytes(StandardCharsets.UTF_8))
        out.close()
        def result = new String(Streams.toByteArray(u.getInputStream()), StandardCharsets.UTF_8)
        then:
        "Hello" == result
    }

    def "test that outputstreams work"() {
        given:
        HttpURLConnection u = new URL("http://localhost:9999/test/os").openConnection()
        when:
        u.setRequestMethod("GET")
        u.setDoInput(true)
        u.setDoOutput(false)
        def arr = Streams.toByteArray(u.getInputStream())
        then:
        9 * 8192 == arr.length

    }

    /**
     * Test an empty POST
     */
    def "Invoke /test/post with empty POST"() {
        given:
        HttpURLConnection u = new URL("http://localhost:9999/test/post").openConnection()
        and:
        def testString = ""
        when:
        u.setRequestMethod("POST")
        u.setRequestProperty("Content-Type",
                             "application/x-www-form-urlencoded")

        u.setRequestProperty("Content-Length", Integer.toString(testString.getBytes().length))
        u.setDoInput(true)
        u.setDoOutput(true)
        def out = u.getOutputStream()
        out.write(testString.getBytes(StandardCharsets.UTF_8))
        out.close()
        def result = new String(Streams.toByteArray(u.getInputStream()), StandardCharsets.UTF_8)
        then:
        "" == result
    }

    /**
     * Ensure that predispatching does not trigger on GET requests
     */
    def "Invoke /test/presidpatch with GET"() {
        given:
        HttpURLConnection u = new URL("http://localhost:9999/test/predispatch").openConnection()
        when:
        u.setRequestMethod("GET")
        then:
        u.getResponseCode() == 404
    }

    def countBytesInStream(InputStream input) {
        def counter = 0
        def count = 0
        def buffer = new byte[8192]
        while ((count = input.read(buffer)) > 0) {
            counter += count
        }

        return counter
    }

    /**
     * Test correct decoding
     */
    def "Invoke /test/json testing correct decoding delimiter"() {
        given:
        def uri = "/test/json?test=Hello%2FWorld"
        def expectedHeaders = ['content-type': 'application/json;charset=UTF-8']
        when:
        def data = callAndRead(uri, null, expectedHeaders)
        then:
        Json.parseObject(data).get("test").asText() == 'Hello/World'
    }

    def "Invoke /test/json testing correct decoding space"() {
        given:
        def uri = "/test/json?test=Hello%20World"
        def expectedHeaders = ['content-type': 'application/json;charset=UTF-8']
        when:
        def data = callAndRead(uri, null, expectedHeaders)
        then:
        Json.parseObject(data).get("test").asText() == 'Hello World'
    }

    def "Invoke /test/json-param testing correct decoding delimiter"() {
        given:
        def uri = "/test/json-param/Hello%2FWorld"
        def expectedHeaders = ['content-type': 'application/json;charset=UTF-8']
        when:
        def data = callAndRead(uri, null, expectedHeaders)
        then:
        Json.parseObject(data).get("test").asText() == 'Hello/World'
    }

    def "Invoke /test/json-param testing correct decoding space"() {
        given:
        def uri = "/test/json-param/Hello%20World"
        def expectedHeaders = ['content-type': 'application/json;charset=UTF-8']
        when:
        def data = callAndRead(uri, null, expectedHeaders)
        then:
        Json.parseObject(data).get("test").asText() == 'Hello World'
    }

    def "Invoke /test/json-params/one/t%2Fwotesting multiple parameter decoding delimiter"() {
        given:
        def uri = "/test/json-params/one/t%2Fwo"
        def expectedHeaders = ['content-type': 'application/json;charset=UTF-8']
        when:
        def data = callAndRead(uri, null, expectedHeaders)
        then:
        Json.parseObject(data).get("param1").asText() == 'one'
        and:
        Json.parseObject(data).get("param2").asText() == 't/wo'
    }

    def "Invoke /test/json-params/one/t%20wo testing multiple parameter decoding space"() {
        given:
        def uri = "/test/json-params/one/t%20wo"
        def expectedHeaders = ['content-type': 'application/json;charset=UTF-8']
        when:
        def data = callAndRead(uri, null, expectedHeaders)
        then:
        Json.parseObject(data).get("param1").asText() == 'one'
        and:
        Json.parseObject(data).get("param2").asText() == 't wo'
    }

    def "Invoke /test/json-param testing param with only delimiter"() {
        given:
        def uri = "/test/json-param/%2F%2F%2F"
        def expectedHeaders = ['content-type': 'application/json;charset=UTF-8']
        when:
        def data = callAndRead(uri, null, expectedHeaders)
        then:
        Json.parseObject(data).get("test").asText() == '///'
    }

    def "Invoke /test/json-param testing param with only space"() {
        given:
        def uri = "/test/json-param/%20%20%20"
        def expectedHeaders = ['content-type': 'application/json;charset=UTF-8']
        when:
        def data = callAndRead(uri, null, expectedHeaders)
        then:
        Json.parseObject(data).get("test").asText() == '   '
    }

    def "testRequest follows redirects if instructed"() {
        when:
        def response1 = TestRequest.GET("/test/redirect-to-get").execute()
        def response2 = TestRequest.GET("/test/redirect-to-get").followRedirect().execute()
        then:
        response1.getType() == TestResponse.ResponseType.TEMPORARY_REDIRECT
        response1.getStatus() == HttpResponseStatus.FOUND
        and:
        response2.getType() == TestResponse.ResponseType.DIRECT
        response2.getStatus() == HttpResponseStatus.OK
    }
}
