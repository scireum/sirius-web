/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

@file:Suppress("DANGEROUS_CHARACTERS")

package sirius.web.service

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
}
