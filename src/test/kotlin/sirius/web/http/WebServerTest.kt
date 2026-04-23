/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

@file:Suppress("DANGEROUS_CHARACTERS")

package sirius.web.http


import io.netty.bootstrap.Bootstrap
import io.netty.channel.ChannelHandlerContext
import io.netty.channel.ChannelInboundHandlerAdapter
import io.netty.channel.ChannelInitializer
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.SocketChannel
import io.netty.channel.socket.nio.NioSocketChannel
import io.netty.handler.codec.http.DefaultFullHttpRequest
import io.netty.handler.codec.http.FullHttpResponse
import io.netty.handler.codec.http.HttpClientCodec
import io.netty.handler.codec.http.HttpHeaderNames
import io.netty.handler.codec.http.HttpMethod
import io.netty.handler.codec.http.HttpObjectAggregator
import io.netty.handler.codec.http.HttpResponseStatus
import io.netty.handler.codec.http.HttpVersion
import org.junit.jupiter.api.Test
import sirius.web.dispatch.TestDispatcher
import org.junit.jupiter.api.assertDoesNotThrow
import org.junit.jupiter.api.assertThrows
import org.junit.jupiter.api.extension.ExtendWith
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.CsvSource
import sirius.kernel.SiriusExtension
import sirius.kernel.commons.Json
import sirius.kernel.commons.Streams
import sirius.kernel.commons.Strings
import sirius.kernel.commons.Wait
import sirius.kernel.health.Log
import sirius.kernel.health.LogHelper
import java.io.IOException
import java.net.HttpURLConnection
import java.net.Socket
import java.net.URI
import java.nio.charset.StandardCharsets
import java.util.Collections
import java.util.concurrent.TimeUnit
import java.util.logging.Level
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertTrue

/**
 * Simulates a bunch of "real" (outside) requests through netty and sirius.
 * <p>
 * This ensures a basic performance profile and also makes sure no trivial race conditions or memory leaks
 * are added.
 */
@ExtendWith(SiriusExtension::class)
class WebServerTest {
    companion object {
        fun callAndRead(
            uri: String,
            outHeaders: Map<String, String?>? = null,
            expectedHeaders: Map<String, String?>? = null,
            requestMethod: String = "GET"
        ): String {
            val connection = URI("http://localhost:9999$uri").toURL().openConnection() as HttpURLConnection
            connection.setRequestMethod(requestMethod)

            outHeaders?.forEach { (key, value) -> connection.addRequestProperty(key, value) }
            connection.connect()
            val result = String(Streams.toByteArray(connection.inputStream), StandardCharsets.UTF_8)
            expectedHeaders?.forEach { (key, value) ->
                if ("*" == value) {
                    if (Strings.isEmpty(connection.getHeaderField(key))) {
                        throw IllegalStateException("Header: $key was expected, but not set")
                    }
                } else if (!Strings.areEqual(connection.getHeaderField(key), value)) {
                    throw IllegalStateException("Header: " + key + " was " + connection.getHeaderField(key) + " instead of " + value)
                }
            }

            return result
        }
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
        assertDoesNotThrow { callAndRead(uri, headers, expectedHeaders) }
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
            callAndRead(uri, null, expectedHeaders)
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
        val connection =
            URI("http://localhost:9999/tunnel/test_transform").toURL().openConnection() as HttpURLConnection
        val transformedData = Streams.toByteArray(connection.inputStream)
        // We un-shift all bytes
        val reTransformedData = ByteArray(transformedData.size)

        for (index in transformedData.indices) {
            reTransformedData[index] =
                if (transformedData[index] == 0.toByte()) 255.toByte() else (transformedData[index] - 1).toByte()
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
     * Tunnels a streaming payload through the back-pressure bridge while the client deliberately
     * reads slowly.
     * <p>
     * The payload exceeds Netty's default high watermark (64 KiB), which forces the bridge to
     * toggle {@code autoRead} on the upstream channel. The test verifies that the full payload
     * arrives intact, i.e. that back-pressure never deadlocks the tunnel and that
     * {@code autoRead} is eventually restored.
     */
    @Test
    fun `Large tunneled payload is delivered completely to a slow consumer`() {
        val connection =
            URI("http://localhost:9999/tunnel/streaming-payload").toURL().openConnection() as HttpURLConnection
        connection.connect()

        var totalBytes = 0
        // Reads in 8 KiB blocks so the artificial per-iteration sleep still exercises the bridge
        // across multiple writability transitions without inflating the test's wall-clock time.
        val chunk = ByteArray(8 * 1024)
        var read: Int
        try {
            connection.inputStream.use { input ->
                while (input.read(chunk).also { read = it } > 0) {
                    totalBytes += read
                    Wait.millis(1)
                }
            }
        } finally {
            connection.disconnect()
        }

        assertEquals(TestDispatcher.STREAMING_PAYLOAD_TOTAL_BYTES, totalBytes)
    }

    /**
     * Sends multiple HTTP/1.1 keep-alive requests over a single Netty channel and waits for each
     * full response before sending the next request.
     * <p>
     * Each response tunnels a payload that exceeds Netty's high watermark.
     * <p>
     * This exercises the full lifecycle of the back-pressure bridge across connection reuse. If the
     * bridge were not properly removed after a response, the next tunnel attempt would cause a
     * {@code DuplicateChannelHandlerNameException} inside the server-side event loop. While Netty
     * swallows that exception, the bridge would not be installed for subsequent requests, leaving
     * them without any back-pressure protection and eventually leading to corrupt or incomplete
     * responses under load.
     */
    @Test
    fun `Sequential keep-alive tunnel requests do not leak the backpressure bridge`() {
        val receivedLengths = Collections.synchronizedList(ArrayList<Int>())
        val workerGroup = NioEventLoopGroup()

        try {
            val bootstrap = Bootstrap()
            bootstrap.group(workerGroup)
            bootstrap.channel(NioSocketChannel::class.java)
            bootstrap.handler(object : ChannelInitializer<SocketChannel>() {
                override fun initChannel(ch: SocketChannel) {
                    ch.pipeline().addLast(HttpClientCodec())
                    // Aggregator must hold the full streaming payload (~20 MiB) per response.
                    ch.pipeline().addLast(HttpObjectAggregator(64 * 1024 * 1024))
                    ch.pipeline().addLast(object : ChannelInboundHandlerAdapter() {
                        override fun channelRead(ctx: ChannelHandlerContext, msg: Any) {
                            if (msg is FullHttpResponse) {
                                receivedLengths.add(msg.content().readableBytes())
                                msg.release()
                                if (receivedLengths.size >= 3) {
                                    ctx.channel().close()
                                }
                            }
                        }
                    })
                }
            })

            val channel = bootstrap.connect("localhost", 9999).sync().channel()
            repeat(3) { requestIndex ->
                val expectedResponses = requestIndex + 1
                channel.writeAndFlush(
                    DefaultFullHttpRequest(
                        HttpVersion.HTTP_1_1,
                        HttpMethod.GET,
                        "/tunnel/streaming-payload"
                    )
                ).sync()

                val deadline = System.currentTimeMillis() + 15_000
                while (receivedLengths.size < expectedResponses && System.currentTimeMillis() < deadline) {
                    Wait.millis(10)
                }
                assertEquals(
                    expectedResponses,
                    receivedLengths.size,
                    "Timed out waiting for response ${requestIndex + 1}"
                )
            }
            channel.close()
            assertTrue(channel.closeFuture().await(10, TimeUnit.SECONDS), "Timed out waiting for client channel close")
        } finally {
            workerGroup.shutdownGracefully()
        }

        assertEquals(3, receivedLengths.size, "Expected exactly 3 responses")
        receivedLengths.forEach { length ->
            assertEquals(TestDispatcher.STREAMING_PAYLOAD_TOTAL_BYTES, length)
        }
    }

    /**
     * Proves that the back-pressure bridge in [TunnelHandler] keeps the server-side outbound
     * buffer bounded when a client stops reading.
     * <p>
     * A raw TCP socket sends a request for a ~20 MiB streaming tunnel response and then never
     * reads anything. While the request is in flight, the test samples the pending bytes in
     * every server-side child channel's outbound buffer via [TestChannelTracker].
     * <p>
     * Without back-pressure, every body part returned by the upstream HTTP client is immediately
     * pushed into the client channel's {@link io.netty.channel.ChannelOutboundBuffer}. Since the
     * stalled client never drains the socket, the buffer grows monotonically toward the full
     * payload size. With back-pressure, once the client channel's buffer crosses the high
     * watermark, upstream {@code autoRead} is flipped off and the pipeline stabilises near the
     * high watermark plus a small amount of slack.
     */
    @Test
    fun `Back-pressure keeps tunnel outbound buffering bounded for a stalled consumer`() {
        // Measures the exact number of bytes currently queued in every server-side child
        // channel's outbound buffer. The tracker attaches to the listening channel's pipeline and
        // therefore covers all HTTP connections to the test WebServer without any modification of
        // production code.
        TestChannelTracker.install()

        Socket("localhost", 9999).use { socket ->
            socket.getOutputStream().apply {
                write(
                    ("GET /tunnel/streaming-payload HTTP/1.1\r\n" +
                            "Host: localhost\r\n" +
                            "Connection: close\r\n\r\n").toByteArray(StandardCharsets.US_ASCII)
                )
                flush()
            }

            // Sample long enough that, without back-pressure, the entire 20 MiB payload would have
            // been handed to the client channel's outbound buffer on localhost.
            var peak = 0L
            val deadline = System.currentTimeMillis() + 5_000
            while (System.currentTimeMillis() < deadline) {
                peak = maxOf(peak, TestChannelTracker.totalPendingOutboundBytes())
                Wait.millis(25)
            }

            // Back-pressure keeps the in-flight buffering at ~ high watermark (64 KiB) plus HTTP
            // framing overhead. Without it, the delta climbs toward the full payload size
            // (~20 MiB). The threshold is intentionally generous for CI jitter.
            val threshold = 1L * 1024 * 1024
            val payload = TestDispatcher.STREAMING_PAYLOAD_TOTAL_BYTES.toLong()
            assertTrue(
                peak < threshold,
                "Peak pending outbound bytes across all server channels: $peak " +
                        "(payload = $payload, threshold = $threshold). " +
                        "This indicates the tunnel is not applying back-pressure to the upstream channel."
            )
        }
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

        val url = URI("http://localhost:9999/test/predispatch").toURL().openConnection() as HttpURLConnection

        val testByteArray = "Hello Service".toByteArray()

        url.setRequestMethod("POST")
        url.setDoInput(true)
        url.setDoOutput(true)
        val output = url.outputStream
        for (index in 1..1024) {
            output.write(testByteArray)
        }
        output.close()
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

        val url = URI("http://localhost:9999/test/predispatch/abort").toURL().openConnection() as HttpURLConnection

        url.setChunkedStreamingMode(1024)

        val testByteArray = "X".toByteArray()

        url.setRequestMethod("POST")
        url.setDoInput(true)
        url.setDoOutput(true)

        // Write some data and flush so that the server triggers a response
        val output = url.outputStream
        for (index in 0..1024) {
            output.write(testByteArray)
        }
        output.flush()

        // Slow down to ensure that the response is created and sent
        // Still no IOException is expected, as the server will discard all data..
        Wait.millis(200)
        for (index in 0..1024) {
            output.write(testByteArray)
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

        val url = URI("http://localhost:9999/test/post").toURL().openConnection() as HttpURLConnection

        val testString = "value=Hello"

        url.setRequestMethod("POST")
        url.setRequestProperty(
            "Content-Type",
            "application/x-www-form-urlencoded"
        )

        url.setRequestProperty("Content-Length", testString.toByteArray().size.toString())
        url.setDoInput(true)
        url.setDoOutput(true)
        val output = url.outputStream
        output.write(testString.toByteArray(StandardCharsets.UTF_8))
        output.close()
        val result = String(Streams.toByteArray(url.inputStream), StandardCharsets.UTF_8)

        assertEquals("Hello", result)
    }

    @Test
    fun `test that outputstreams work`() {

        val url = URI("http://localhost:9999/test/os").toURL().openConnection() as HttpURLConnection

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

        val url = URI("http://localhost:9999/test/post").toURL().openConnection() as HttpURLConnection

        val testString = ""

        url.setRequestMethod("POST")
        url.setRequestProperty(
            "Content-Type",
            "application/x-www-form-urlencoded"
        )

        url.setRequestProperty("Content-Length", testString.toByteArray().size.toString())
        url.setDoInput(true)
        url.setDoOutput(true)
        val output = url.outputStream
        output.write(testString.toByteArray(StandardCharsets.UTF_8))
        output.close()
        val result = String(Streams.toByteArray(url.inputStream), StandardCharsets.UTF_8)

        assertEquals("", result)
    }

    /**
     * Ensure that predispatching does not trigger on GET requests
     */
    @Test
    fun `Invoke test-presidpatch with GET`() {

        val url = URI("http://localhost:9999/test/predispatch").toURL().openConnection() as HttpURLConnection

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

    @ParameterizedTest
    @CsvSource(
        delimiter = '|', useHeadersInDisplayName = true, textBlock = // language=CSV
            """
            uri                             | method | result
            /test/restricted-method         | GET    | GET OK
            /test/restricted-method         | POST   | POST OK
            /test/restricted-methods        | GET    | GET/POST OK
            /test/restricted-methods        | POST   | GET/POST OK
            /test/another-restricted-method | GET    | GET OK"""
    )
    fun `Requests to plain text routes with restricted method work`(uri: String, method: String, result: String) {
        val data = callAndRead(uri, null, null, method)
        assertEquals(result, data)
    }

    @ParameterizedTest
    @CsvSource(
        delimiter = '|', useHeadersInDisplayName = true, textBlock = // language=CSV
            """
            uri                             | method | allow
            /test/restricted-method         | PUT    | GET, POST
            /test/restricted-method         | DELETE | GET, POST
            /test/restricted-methods        | PUT    | GET, POST
            /test/restricted-methods        | DELETE | GET, POST
            /test/another-restricted-method | POST   | GET"""
    )
    fun `Requests to plain text routes with wrong method fail with 405`(uri: String, method: String, allow: String) {
        val connection = URI("http://localhost:9999$uri").toURL().openConnection() as HttpURLConnection
        connection.setRequestMethod(method)

        assertEquals(405, connection.responseCode)
        assertEquals(allow, connection.getHeaderField("Allow"))
    }

    @ParameterizedTest
    @CsvSource(
        delimiter = '|', useHeadersInDisplayName = true, textBlock = // language=CSV
            """
            uri                                 | method | result
            /test/restricted-method-api         | GET    | GET OK
            /test/restricted-method-api         | POST   | POST OK
            /test/restricted-methods-api        | GET    | GET/POST OK
            /test/restricted-methods-api        | POST   | GET/POST OK
            /test/another-restricted-method-api | GET    | GET OK"""
    )
    fun `Requests to JSON routes with restricted method work`(uri: String, method: String, result: String) {
        val data = callAndRead(uri, null, null, method)
        assertEquals(result, Json.parseObject(data).get("status").asText())
    }

    @ParameterizedTest
    @CsvSource(
        delimiter = '|', useHeadersInDisplayName = true, textBlock = // language=CSV
            """
            uri                                 | method | allow
            /test/restricted-method-api         | PUT    | GET, POST
            /test/restricted-method-api         | DELETE | GET, POST
            /test/restricted-methods-api        | PUT    | GET, POST
            /test/restricted-methods-api        | DELETE | GET, POST
            /test/another-restricted-method-api | POST   | GET"""
    )
    fun `Requests to JSON routes with wrong method fail with 405`(uri: String, method: String, allow: String) {
        val connection = URI("http://localhost:9999$uri").toURL().openConnection() as HttpURLConnection
        connection.setRequestMethod(method)

        assertEquals(405, connection.responseCode)
        assertEquals(allow, connection.getHeaderField("Allow"))

        val result = Json.parseObject(String(Streams.toByteArray(connection.errorStream), StandardCharsets.UTF_8))
        assertTrue(result.get("error").asBoolean())
        assertFalse(result.get("success").asBoolean())
    }

    @ParameterizedTest
    @CsvSource(
        delimiter = '|', useHeadersInDisplayName = true, textBlock = // language=CSV
            """
            uri
            /test/without-method
            /test/without-method-api"""
    )
    fun `Requests to all routes without methods defined fail with 404`(uri: String) {
        val connection = URI("http://localhost:9999$uri").toURL().openConnection() as HttpURLConnection
        connection.setRequestMethod("GET")

        assertEquals(404, connection.responseCode)
    }
}
