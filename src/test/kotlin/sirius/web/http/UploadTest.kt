/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.http

import org.junit.jupiter.api.Tag
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.extension.ExtendWith
import sirius.kernel.SiriusExtension
import sirius.kernel.Tags
import sirius.kernel.commons.Json
import sirius.kernel.commons.Streams

import java.io.File
import java.io.FileInputStream
import java.net.HttpURLConnection
import java.net.URI
import java.net.URL
import java.nio.charset.StandardCharsets
import kotlin.test.assertEquals

/**
 * Simulates "real" uploads through netty and sirius.
 */
@Tag(Tags.NIGHTLY)
@ExtendWith(SiriusExtension::class)
class UploadTest {

    fun `upload`(uri: String, file: File): String {
        val connection = URI("http://localhost:9999" + uri).toURL().openConnection() as HttpURLConnection
        val inputStream = FileInputStream(file)
        connection.setDoOutput(true)
        connection.setRequestMethod("POST")
        val outputStream = connection.getOutputStream()
        Streams.transfer(inputStream, outputStream)
        inputStream.close()
        outputStream.flush()

        return String(Streams.toByteArray(connection.getInputStream()), StandardCharsets.UTF_8)
    }

    @Test
    fun `Uploads a file to upload-test`() {
        val file = File("src/test/resources/test.png")
        val response = upload("/upload-test", file)
        val json = Json.parseObject(response)
        assertEquals(true, json.get("success").asBoolean())
        assertEquals(file.length(), json.get("size").asLong())
    }

    @Test
    fun `Uploads a gzip-file to upload-gzip`() {
        val file = File("src/test/resources/test.csv.gz")
        val response = upload("/upload-gzip", file)
        val json = Json.parseObject(response)
        assertEquals(true, json.get("success").asBoolean())
        assertEquals(3, json.get("lines").asLong())
    }
}
