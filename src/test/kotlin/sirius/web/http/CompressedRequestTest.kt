/*
 * Made with all the love in the world
 * by scireum in Stuttgart, Germany
 *
 * Copyright by scireum GmbH
 * https://www.scireum.de - info@scireum.de
 */

package sirius.web.http

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.extension.ExtendWith
import sirius.kernel.SiriusExtension
import sirius.kernel.commons.Json
import sirius.kernel.commons.Streams
import java.io.ByteArrayOutputStream
import java.net.HttpURLConnection
import java.net.URI
import java.nio.charset.StandardCharsets
import java.util.zip.GZIPOutputStream
import kotlin.test.assertEquals
import kotlin.test.assertTrue

/**
 * Ensures that services decode request bodies sent with an accepted `Content-Encoding`, and reject all others as
 * specified by RFC 7694.
 *
 * The requests are sent through netty, as [TestRequest] bypasses the receiving of the body.
 */
@ExtendWith(SiriusExtension::class)
class CompressedRequestTest {

    companion object {
        private val PAYLOAD = "a".repeat(100_000).toByteArray()
    }

    @Test
    fun `gzip body is decompressed for mapped services`() {
        val response =
            send("/test/mapped/greet", "application/json", "gzip", gzip("""{"name": "World"}""".toByteArray()))

        assertEquals(HttpURLConnection.HTTP_OK, response.status)
        assertEquals("""{"greeting":"Hello World"}""", response.body)
    }

    @Test
    fun `gzip body is decompressed for services`() {
        val response = send("/test/service-content", "text/plain", "gzip", gzip(PAYLOAD))

        assertEquals(HttpURLConnection.HTTP_OK, response.status)
        val json = Json.parseObject(response.body)
        assertEquals(PAYLOAD.size.toLong(), json.get("size").asLong())
        // The header is removed, as it no longer describes the body...
        assertTrue(json.get("contentEncoding").isNull, response.body)
    }

    @Test
    fun `chunked gzip body is decompressed and described by its length`() {
        val response = send("/test/service-content", "text/plain", "gzip", gzip(PAYLOAD), chunked = true)

        assertEquals(HttpURLConnection.HTTP_OK, response.status)
        val json = Json.parseObject(response.body)
        assertEquals(PAYLOAD.size.toLong(), json.get("size").asLong())
        assertTrue(json.get("transferEncoding").isNull, response.body)
    }

    @Test
    fun `x-gzip is accepted as alias of gzip`() {
        val response = send("/test/service-content", "text/plain", "x-gzip", gzip(PAYLOAD))

        assertEquals(HttpURLConnection.HTTP_OK, response.status)
        assertEquals(PAYLOAD.size.toLong(), response.size())
    }

    @Test
    fun `identity is accepted by all services`() {
        val response = send("/test/uncompressed-service-content", "text/plain", "identity", PAYLOAD)

        assertEquals(HttpURLConnection.HTTP_OK, response.status)
        assertEquals(PAYLOAD.size.toLong(), response.size())
    }

    @Test
    fun `gzip body exceeding http-maxDecompressedRequestSize once decompressed is rejected`() {
        // The body is only a few KB compressed, but exceeds the limit of 10M once decompressed...
        val body = gzip(ByteArray(11 * 1024 * 1024))

        val response = send("/test/service-content", "application/octet-stream", "gzip", body)

        assertEquals(HttpURLConnection.HTTP_ENTITY_TOO_LARGE, response.status)
        assertTrue(response.body.contains("exceeds the maximal size"), response.body)
    }

    @Test
    fun `invalid gzip body is rejected`() {
        val response = send("/test/service-content", "application/octet-stream", "gzip", "not gzip".toByteArray())

        assertEquals(HttpURLConnection.HTTP_BAD_REQUEST, response.status)
        assertTrue(response.body.contains("not valid data for the content encoding 'gzip'"), response.body)
    }

    @Test
    fun `encoding not accepted by the service is rejected with the accepted encodings`() {
        val response = send("/test/service-content", "application/octet-stream", "deflate", "data".toByteArray())

        assertEquals(HttpURLConnection.HTTP_UNSUPPORTED_TYPE, response.status)
        assertEquals("gzip", response.acceptEncoding)
        assertTrue(response.body.contains("'deflate' is not supported"), response.body)
    }

    @Test
    fun `stacked encodings are rejected`() {
        val body = gzip(gzip(ByteArray(1)))

        val response = send("/test/service-content", "application/octet-stream", "gzip, gzip", body)

        assertEquals(HttpURLConnection.HTTP_UNSUPPORTED_TYPE, response.status)
        assertEquals("gzip", response.acceptEncoding)
    }

    @Test
    fun `gzip form body is rejected, as it is parsed while being received`() {
        val response =
            send("/test/service-content", "application/x-www-form-urlencoded", "gzip", gzip("a=b".toByteArray()))

        assertEquals(HttpURLConnection.HTTP_UNSUPPORTED_TYPE, response.status)
        assertEquals("identity", response.acceptEncoding)
        assertTrue(response.body.contains("Form bodies cannot be sent with a content encoding"), response.body)
    }

    @Test
    fun `gzip body is rejected by services which do not accept encodings`() {
        val response =
            send("/test/uncompressed-service-content", "application/octet-stream", "gzip", gzip(ByteArray(1)))

        assertEquals(HttpURLConnection.HTTP_UNSUPPORTED_TYPE, response.status)
        assertEquals("identity", response.acceptEncoding)
    }

    @Test
    fun `gzip body is passed on unchanged to other routes`() {
        val body = gzip(PAYLOAD)

        val response = send("/test/raw-content", "application/octet-stream", "gzip", body)

        assertEquals(HttpURLConnection.HTTP_OK, response.status)
        assertEquals(body.size.toString(), response.body)
    }

    @Test
    fun `gzip body is passed on unchanged to pre-dispatched services`() {
        val body = gzip(PAYLOAD)

        val response = send("/upload-test", "application/octet-stream", "gzip", body)

        assertEquals(HttpURLConnection.HTTP_OK, response.status)
        assertEquals(body.size.toLong(), response.size())
    }

    private data class Response(val status: Int, val body: String, val acceptEncoding: String?) {
        fun size() = Json.parseObject(body).get("size").asLong()
    }

    private fun send(
        uri: String,
        contentType: String,
        encoding: String,
        body: ByteArray,
        chunked: Boolean = false
    ): Response {
        val connection = URI("http://localhost:9999$uri").toURL().openConnection() as HttpURLConnection
        connection.requestMethod = "POST"
        connection.doOutput = true
        if (chunked) {
            connection.setChunkedStreamingMode(1024)
        }
        connection.setRequestProperty("Content-Type", contentType)
        connection.setRequestProperty("Content-Encoding", encoding)
        connection.outputStream.use { it.write(body) }

        val status = connection.responseCode
        val stream = if (status < 400) connection.inputStream else connection.errorStream
        return Response(
            status,
            String(Streams.toByteArray(stream), StandardCharsets.UTF_8),
            connection.getHeaderField("Accept-Encoding")
        )
    }

    private fun gzip(data: ByteArray): ByteArray {
        val buffer = ByteArrayOutputStream()
        GZIPOutputStream(buffer).use { it.write(data) }
        return buffer.toByteArray()
    }
}
