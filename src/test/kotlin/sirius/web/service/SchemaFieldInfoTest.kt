/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

@file:Suppress("DANGEROUS_CHARACTERS")

package sirius.web.service

import io.swagger.v3.oas.annotations.media.Schema
import sirius.web.controller.GreetInput
import sirius.web.controller.GreetResult
import sirius.web.services.SchemaFieldInfo
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertTrue
import java.lang.reflect.Type

/**
 * Tests the introspection of [SchemaFieldInfo] used to auto-document mapped services.
 */
class SchemaFieldInfoTest {

    @Test
    fun `derives required field information from Schema annotations`() {
        val fields = SchemaFieldInfo.forType(GreetInput::class.java)

        assertEquals(1, fields.size)
        val name = fields.first()
        assertEquals("name", name.name)
        assertEquals("String", name.type)
        assertTrue(name.isRequired)
        assertEquals("The name of the person to greet", name.description)
        assertEquals("World", name.example)
    }

    @Test
    fun `treats optional fields as not required`() {
        val fields = SchemaFieldInfo.forType(GreetResult::class.java)

        assertEquals(1, fields.size)
        assertEquals("greeting", fields.first().name)
        assertFalse(fields.first().isRequired)
    }

    @Test
    fun `returns an empty list for null or annotation-less types`() {
        assertTrue(SchemaFieldInfo.forType(null).isEmpty())
        assertTrue(SchemaFieldInfo.forType(String::class.java).isEmpty())
    }

    @Test
    fun `recursively documents annotated nested fields and collection elements`() {
        val fields = SchemaFieldInfo.forType(NestedResponse::class.java)

        assertEquals(listOf("items", "items[].code", "itemsById", "itemsById[].code", "page"), fields.map { it.name })
        assertEquals("List<NestedItem>", fields[0].type)
        assertEquals("String", fields[1].type)
        assertEquals("Map<String, NestedItem>", fields[2].type)
    }

    @Test
    fun `stops recursion for self-referential types`() {
        val fields = SchemaFieldInfo.forType(RecursiveResponse::class.java)

        assertEquals(listOf("name", "child"), fields.map { it.name })
    }

    @Test
    fun `binds type variables declared by a generic superclass`() {
        val fields = SchemaFieldInfo.forType(TypedListResponse::class.java)

        assertEquals(listOf("items", "items[].code"), fields.map { it.name })
        assertEquals("List<NestedItem>", fields.first().type)
    }

    @Test
    fun `documents elements of top-level collections`() {
        val fields = SchemaFieldInfo.forType(topLevelItemsType())

        assertEquals(listOf("code"), fields.map { it.name })
        assertEquals("String", fields.first().type)
    }

    private fun topLevelItemsType(): Type = TopLevelCollection::class.java.getDeclaredField("items").genericType

    // The fixture properties are only read via reflection by SchemaFieldInfo, hence the unused suppression.

    @Suppress("unused")
    private class NestedResponse(
        @field:Schema(description = "Result items")
        val items: List<NestedItem>,
        @field:Schema(description = "Result items by identifier")
        val itemsById: Map<String, NestedItem>,
        @field:Schema(description = "Current page")
        val page: Int
    )

    @Suppress("unused")
    private class NestedItem(
        @field:Schema(description = "Item code")
        val code: String
    )

    @Suppress("unused")
    private class RecursiveResponse(
        @field:Schema(description = "Display name")
        val name: String,
        @field:Schema(description = "Child response")
        val child: RecursiveResponse?
    )

    private class TopLevelCollection {
        @Suppress("unused")
        lateinit var items: List<NestedItem>
    }

    @Suppress("unused")
    private open class GenericBaseResponse<T>(
        @field:Schema(description = "Result items")
        val items: List<T>
    )

    private class TypedListResponse(items: List<NestedItem>) : GenericBaseResponse<NestedItem>(items)
}
