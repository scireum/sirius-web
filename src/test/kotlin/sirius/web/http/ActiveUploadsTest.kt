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
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.ValueSource
import sirius.kernel.SiriusExtension
import sirius.kernel.commons.Wait
import sirius.web.controller.TestController
import java.io.BufferedReader
import java.io.InputStreamReader
import java.net.Socket
import java.nio.charset.StandardCharsets
import java.util.concurrent.TimeUnit
import kotlin.test.assertEquals
import kotlin.test.assertTrue

/**
 * Ensures that [WebServer.countActiveUploads] reports uploads while they are in progress, and only then.
 */
@ExtendWith(SiriusExtension::class)
class ActiveUploadsTest {

    @ParameterizedTest
    @ValueSource(strings = ["/upload-test", "/test/fake-delete-data-unsafe"])
    fun `an upload is counted until its response is completed`(uri: String) {
        Socket("localhost", 9999).use { socket ->
            sendPostHeader(socket, uri, 1024)
            sendBody(socket, 16)
            awaitActiveUploads(1)

            sendBody(socket, 1024 - 16)
            val statusLine = readStatusLine(socket)
            assertTrue(statusLine.contains("200"), statusLine)

            // The connection is kept alive, so that only the completed response can end the upload...
            awaitActiveUploads(0)
            assertTrue(WebServer.getOpenConnections().any { it.url == "$uri (completed)" })
        }
    }

    @ParameterizedTest
    @ValueSource(strings = ["/upload-delayed", "/upload-delayed-buffered"])
    fun `an upload is still counted while it is processed after its last byte`(uri: String) {
        Socket("localhost", 9999).use { socket ->
            sendPostHeader(socket, uri, 1024)
            sendBody(socket, 1024)

            // The route has received the whole body and now holds back its response...
            assertTrue(TestController.DELAYED_UPLOAD_RECEIVED.tryAcquire(5, TimeUnit.SECONDS))
            assertEquals(1L, WebServer.countActiveUploads())

            TestController.DELAYED_UPLOAD_RELEASED.release()
            val statusLine = readStatusLine(socket)
            assertTrue(statusLine.contains("200"), statusLine)
            awaitActiveUploads(0)
        }
    }

    @Test
    fun `an upload is still counted while its response is being written`() {
        Socket("localhost", 9999).use { socket ->
            sendPostHeader(socket, "/upload-large-response", 1024, close = true)
            sendBody(socket, 1024)

            // The response has been handed over to netty, but as nothing is read yet, it cannot be written completely...
            assertTrue(TestController.LARGE_RESPONSE_HANDED_OVER.tryAcquire(5, TimeUnit.SECONDS))
            assertEquals(1L, WebServer.countActiveUploads())

            val response = socket.getInputStream().readAllBytes()
            assertTrue(response.size > TestController.LARGE_RESPONSE_SIZE)
            assertTrue(String(response, 0, 12, StandardCharsets.US_ASCII).startsWith("HTTP/1.1 200"))
            awaitActiveUploads(0)
        }
    }

    @Test
    fun `an aborted upload is no longer counted`() {
        // A buffered route is used here, as a pre-dispatched one would keep its worker waiting for the missing
        // data until the read timeout of the InputStreamHandler expires...
        Socket("localhost", 9999).use { socket ->
            sendPostHeader(socket, "/test/fake-delete-data-unsafe", 1024)
            sendBody(socket, 16)
            awaitActiveUploads(1)
        }

        awaitActiveUploads(0)
    }

    companion object {
        private fun sendPostHeader(socket: Socket, uri: String, contentLength: Int, close: Boolean = false) {
            socket.getOutputStream().apply {
                write(
                    ("POST $uri HTTP/1.1\r\n" +
                            "Host: localhost\r\n" +
                            "Content-Type: application/octet-stream\r\n" +
                            (if (close) "Connection: close\r\n" else "") +
                            "Content-Length: $contentLength\r\n\r\n").toByteArray(StandardCharsets.US_ASCII)
                )
                flush()
            }
        }

        private fun sendBody(socket: Socket, length: Int) {
            socket.getOutputStream().apply {
                write(ByteArray(length))
                flush()
            }
        }

        private fun readStatusLine(socket: Socket): String {
            return BufferedReader(InputStreamReader(socket.getInputStream(), StandardCharsets.US_ASCII)).readLine()
        }

        private fun awaitActiveUploads(expected: Long) {
            val deadline = System.currentTimeMillis() + 5_000
            while (WebServer.countActiveUploads() != expected && System.currentTimeMillis() < deadline) {
                Wait.millis(25)
            }
            assertEquals(expected, WebServer.countActiveUploads())
        }
    }
}
