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
import java.io.BufferedReader
import java.io.InputStreamReader
import java.net.Socket
import java.nio.charset.StandardCharsets
import kotlin.test.assertEquals
import kotlin.test.assertTrue

/**
 * Ensures that [WebServer.countActiveUploads] reports uploads while they are in progress, and only then.
 */
@ExtendWith(SiriusExtension::class)
class ActiveUploadsTest {

    companion object {
        private fun sendPostHeader(socket: Socket, uri: String, contentLength: Int) {
            socket.getOutputStream().apply {
                write(
                    ("POST $uri HTTP/1.1\r\n" +
                            "Host: localhost\r\n" +
                            "Content-Type: application/octet-stream\r\n" +
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
}
