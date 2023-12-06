/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.service


import sirius.kernel.xml.Attribute
import sirius.web.services.JSONStructuredOutput
import java.io.ByteArrayOutputStream

import kotlin.test.Test
import kotlin.test.assertEquals

/**
 * Tests the [JSONStructuredOutput] class.
 */
class JsonOutputTest {
    @Test
    fun `json output uses attributes`() {
        val outputStream = ByteArrayOutputStream()
        val jsonOutput = JSONStructuredOutput(outputStream, null, "UTF8");
        jsonOutput.beginResult("test")
        jsonOutput.beginObject("1", Attribute.set("a", "b"))
        jsonOutput.endObject()
        jsonOutput.endResult()
        assertEquals("""{"1":{"a":"b"}}""", outputStream.toString())
    }
}
