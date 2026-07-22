/*
 * Made with all the love in the world
 * by scireum in Stuttgart, Germany
 *
 * Copyright by scireum GmbH
 * https://www.scireum.de - info@scireum.de
 */

@file:Suppress("DANGEROUS_CHARACTERS")

package sirius.web.service

import com.fasterxml.jackson.databind.JsonNode
import io.swagger.v3.oas.annotations.media.Schema
import sirius.web.services.OpenApiGenerator
import kotlin.test.Test
import kotlin.test.assertEquals

/**
 * Tests the nested schema generation of the [OpenApiGenerator] used to build downloadable OpenAPI documents.
 */
class OpenApiGeneratorTest {

    @Test
    fun `describes complex types as component schemas referenced via ref`() {
        val generator = OpenApiGenerator()

        val root = generator.schemaForType(SearchResponse::class.java)

        assertEquals("#/components/schemas/SearchResponse", ref(root))

        val schemas = generator.componentSchemas
        assertEquals(true, schemas.containsKey("SearchResponse"))
        assertEquals(true, schemas.containsKey("SearchItem"))

        val properties = schemas["SearchResponse"]!!.get("properties")
        assertEquals("integer", properties.get("totalHits").get("type").asText())
        assertEquals("boolean", properties.get("hasMore").get("type").asText())

        assertEquals("array", properties.get("items").get("type").asText())
        assertEquals("#/components/schemas/SearchItem", ref(properties.get("items").get("items")))

        assertEquals("object", properties.get("itemsById").get("type").asText())
        assertEquals("#/components/schemas/SearchItem", ref(properties.get("itemsById").get("additionalProperties")))
    }

    @Test
    fun `maps primitives, enums and required fields`() {
        val generator = OpenApiGenerator()

        generator.schemaForType(SearchResponse::class.java)
        val itemSchema = generator.componentSchemas["SearchItem"]!!
        val itemProperties = itemSchema.get("properties")

        assertEquals("number", itemProperties.get("price").get("type").asText())
        assertEquals("Item price", itemProperties.get("price").get("description").asText())

        val availability = itemProperties.get("availability")
        assertEquals("string", availability.get("type").asText())
        assertEquals(listOf("IN_STOCK", "OUT_OF_STOCK"), availability.get("enum").map { it.asText() })

        assertEquals(listOf("code"), itemSchema.get("required").map { it.asText() })
    }

    @Test
    fun `terminates recursion for self-referential types`() {
        val generator = OpenApiGenerator()

        generator.schemaForType(TreeNode::class.java)
        val nodeSchema = generator.componentSchemas["TreeNode"]!!

        assertEquals("#/components/schemas/TreeNode", ref(nodeSchema.get("properties").get("children").get("items")))
    }

    @Test
    fun `uses distinct component names for generic type arguments`() {
        val generator = OpenApiGenerator()

        generator.schemaForType(GenericResponses::class.java)

        val schemas = generator.componentSchemas
        val pages = schemas.filterKeys { it.contains("Page") }
        assertEquals(3, pages.size)
        assertEquals(3, pages.values.map { ref(it.get("properties").get("items").get("items")) }.toSet().size)
    }

    @Test
    fun `writes numeric Schema examples as JSON numbers`() {
        val generator = OpenApiGenerator()

        generator.schemaForType(NumericExample::class.java)

        val example = generator.componentSchemas["NumericExample"]!!
            .get("properties")
            .get("page")
            .get("example")
        assertEquals(true, example.isIntegralNumber)
        assertEquals(1, example.intValue())
    }

    private fun ref(node: JsonNode): String = node.get("\$ref").asText()

    @Suppress("unused")
    private enum class Availability { IN_STOCK, OUT_OF_STOCK }

    @Suppress("unused")
    private class SearchItem(
        @field:Schema(description = "Item code", requiredMode = Schema.RequiredMode.REQUIRED)
        val code: String,
        @field:Schema(description = "Item price")
        val price: Double,
        @field:Schema(description = "Availability")
        val availability: Availability
    )

    @Suppress("unused")
    private class SearchResponse(
        @field:Schema(description = "Total number of hits")
        val totalHits: Int,
        @field:Schema(description = "Whether more results are available")
        val hasMore: Boolean,
        @field:Schema(description = "The result items")
        val items: List<SearchItem>,
        @field:Schema(description = "The result items by identifier")
        val itemsById: Map<String, SearchItem>
    )

    @Suppress("unused")
    private class TreeNode(
        @field:Schema(description = "Display name")
        val name: String,
        @field:Schema(description = "Child nodes")
        val children: List<TreeNode>
    )

    @Suppress("unused")
    private class GenericResponses(
        @field:Schema val articles: Page<Article>,
        @field:Schema val customers: Page<Customer>,
        @field:Schema val orders: Page<Order>
    )

    @Suppress("unused")
    private class Page<T>(
        @field:Schema val items: List<T>
    )

    @Suppress("unused")
    private class Article(@field:Schema val id: String)

    @Suppress("unused")
    private class Customer(@field:Schema val id: String)

    @Suppress("unused")
    private class Order(@field:Schema val id: String)

    @Suppress("unused")
    private class NumericExample(
        @field:Schema(example = "1") val page: Int
    )
}
