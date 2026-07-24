/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

@file:Suppress("DANGEROUS_CHARACTERS")

package sirius.web.service

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.extension.ExtendWith
import sirius.kernel.SiriusExtension
import sirius.kernel.commons.Json
import sirius.kernel.commons.Streams
import sirius.web.controller.GreetInput
import sirius.web.controller.GreetResult
import sirius.web.controller.TestController
import java.net.HttpURLConnection
import java.net.URI
import java.nio.charset.StandardCharsets
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertTrue

/**
 * Tests the mapped input/output service mechanism (request body deserialized into a POJO, result serialized directly)
 * as implemented for [GreetInput], [GreetResult] and the matching routes in [TestController].
 */
@ExtendWith(SiriusExtension::class)
class MappedServiceTest {

    private fun call(
        uri: String,
        body: String? = null,
        method: String = if (body == null) "GET" else "POST"
    ): Triple<Int, String, String?> {
        val connection = URI("http://localhost:9999$uri").toURL().openConnection() as HttpURLConnection
        connection.requestMethod = method
        if (body != null) {
            connection.doOutput = true
            connection.setRequestProperty("Content-Type", "application/json")
            connection.outputStream.use { it.write(body.toByteArray(StandardCharsets.UTF_8)) }
        }
        val status = connection.responseCode
        val stream = if (status >= 400) connection.errorStream else connection.inputStream
        val data = String(Streams.toByteArray(stream), StandardCharsets.UTF_8)
        return Triple(status, data, connection.getHeaderField("content-type"))
    }

    @Test
    fun `mapped service deserializes the request body and serializes the result without envelope`() {
        val (status, data, contentType) = call("/test/mapped/greet", """{"name": "World"}""")

        assertEquals(HttpURLConnection.HTTP_OK, status)
        assertEquals("application/json;charset=UTF-8", contentType)
        // The result POJO is serialized directly - there is no success/error envelope.
        assertEquals("""{"greeting":"Hello World"}""", data)
        assertFalse(data.contains("success"))
    }

    @Test
    fun `mapped service without a request body binds path parameters`() {
        val (status, data, _) = call("/test/mapped/echo/Hello")

        assertEquals(HttpURLConnection.HTTP_OK, status)
        assertEquals("Hello", Json.parseObject(data).get("greeting").asString(""))
    }

    @Test
    fun `mapped service supports asynchronous results via a Promise`() {
        val (status, data, _) = call("/test/mapped/async", """{"name": "Async"}""")

        assertEquals(HttpURLConnection.HTTP_OK, status)
        assertEquals("Hello Async", Json.parseObject(data).get("greeting").asString(""))
    }

    @Test
    fun `a failing mapped service yields a proper JSON error`() {
        val (status, data, _) = call("/test/mapped/fail", """{"name": "World"}""")

        assertEquals(HttpURLConnection.HTTP_BAD_REQUEST, status)
        val json = Json.parseObject(data)
        assertFalse(json.get("success").asBoolean())
        assertTrue(json.get("error").asBoolean())
        assertTrue(json.get("message").asString("").contains("World"))
    }

    @Test
    fun `legacy streaming services still wrap their result in an envelope`() {
        val (status, data, _) = call("/test/json?test=Hello_World")

        assertEquals(HttpURLConnection.HTTP_OK, status)
        // Regression: the legacy StructuredOutput path keeps the success/error envelope.
        assertEquals("""{"success":true,"error":false,"test":"Hello_World"}""", data)
    }
}
