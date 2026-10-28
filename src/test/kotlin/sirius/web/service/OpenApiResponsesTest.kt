/*
 * Made with all the love in the world
 * by scireum in Stuttgart, Germany
 *
 * Copyright by scireum GmbH
 * https://www.scireum.de - info@scireum.de
 */

@file:Suppress("DANGEROUS_CHARACTERS")

package sirius.web.service

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.extension.ExtendWith
import sirius.kernel.SiriusExtension
import sirius.kernel.di.std.Part
import sirius.pasta.tagliatelle.Tagliatelle
import sirius.web.services.OpenApiGenerator
import sirius.web.services.PublicApiInfo
import sirius.web.services.PublicServiceInfo
import sirius.web.services.PublicServices
import tools.jackson.databind.JsonNode
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertTrue

/**
 * Tests how the documented responses of a public service and the response derived from a mapped service are combined,
 * using the services of [sirius.web.services.OpenApiTestController].
 */
@ExtendWith(SiriusExtension::class)
class OpenApiResponsesTest {

    private fun api(): PublicApiInfo = publicServices.apis.first { it.apiName == "openapi-test" }

    private fun service(uri: String): PublicServiceInfo =
        api().sections.flatMap { it.services }.first { it.uri == uri }

    private fun responses(uri: String): JsonNode {
        val document = OpenApiGenerator().generate(api())
        return document.get("paths").get(uri).values().first().get("responses")
    }

    @Test
    fun `documented error responses complement the derived successful response`() {
        val responses = responses("/test/openapi/error-responses")

        assertEquals(
            "#/components/schemas/GreetResult",
            responses.get("200").get("content").get("application/json").get("schema").get("\$ref").asString("")
        )
        assertEquals(listOf("200", "400", "401", "403", "404", "405", "500"), responses.propertyNames().toList())
        assertFalse(service("/test/openapi/error-responses").outputSchema.isEmpty())
    }

    @Test
    fun `a response of the service replaces the shared response with the same status code`() {
        val service = service("/test/openapi/error-responses")

        assertEquals(1, service.responses.count { it.responseCode == "403" })
        assertEquals(
            "The greeting is not permitted:\n- `greeting_forbidden`: the name must not be greeted",
            responses("/test/openapi/error-responses").get("403").get("description").asString("")
        )
    }

    @Test
    fun `the API explorer renders the descriptions of the responses as Markdown`() {
        val html = tagliatelle.resolve("/templates/system/api/service-responses.html.pasta")
            .orElseThrow()
            .renderToString(service("/test/openapi/error-responses"))

        assertTrue(html.contains("<li><code>greeting_forbidden</code>: the name must not be greeted</li>"))
    }

    @Test
    fun `a documented successful response replaces the derived one`() {
        val service = service("/test/openapi/success-response")
        val responses = responses("/test/openapi/success-response")

        assertTrue(service.hasDocumentedSuccessResponse())
        assertEquals("The greeting as plain text", responses.get("200").get("description").asString(""))
        assertTrue(responses.get("200").get("content").has("text/plain"))
        assertTrue(responses.has("500"))
    }

    @Test
    fun `a legacy service documenting only error responses gets a successful response without content`() {
        val service = service("/test/openapi/legacy-error-responses")
        val responses = responses("/test/openapi/legacy-error-responses")

        assertFalse(service.hasDocumentedSuccessResponse())
        assertTrue(service.outputSchema.isEmpty())
        assertEquals("Successful response", responses.get("200").get("description").asString(""))
        assertFalse(responses.get("200").has("content"))
        assertEquals(listOf("200", "400", "401", "403", "404", "405", "500"), responses.propertyNames().toList())
    }

    @Test
    fun `ranges of status codes are error responses and follow the specific status codes`() {
        val service = service("/test/openapi/range-responses")
        val responses = responses("/test/openapi/range-responses")

        assertFalse(service.hasDocumentedSuccessResponse())
        assertTrue(responses.get("200").get("content").has("application/json"))
        assertEquals(listOf("200", "404", "5XX", "4XX"), responses.propertyNames().toList())
    }

    @Test
    fun `a default response is a documented successful response and follows the specific status codes`() {
        val service = service("/test/openapi/default-response")
        val responses = responses("/test/openapi/default-response")

        assertTrue(service.hasDocumentedSuccessResponse())
        assertEquals(listOf("400", "default"), responses.propertyNames().toList())
    }

    companion object {
        @Part
        @JvmStatic
        private lateinit var publicServices: PublicServices

        @Part
        @JvmStatic
        private lateinit var tagliatelle: Tagliatelle
    }
}
