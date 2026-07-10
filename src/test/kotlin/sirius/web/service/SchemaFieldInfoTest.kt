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

    private class NestedResponse(
        @field:Schema(description = "Result items")
        val items: List<NestedItem>,
        @field:Schema(description = "Result items by identifier")
        val itemsById: Map<String, NestedItem>,
        @field:Schema(description = "Current page")
        val page: Int
    )

    private class NestedItem(
        @field:Schema(description = "Item code")
        val code: String
    )

    private class RecursiveResponse(
        @field:Schema(description = "Display name")
        val name: String,
        @field:Schema(description = "Child response")
        val child: RecursiveResponse?
    )
}
