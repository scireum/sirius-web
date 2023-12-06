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

class JsonOutputTest {
    @Test
    fun `json output uses attributes`() {
        val os =  ByteArrayOutputStream();
        val out =  JSONStructuredOutput(os, null, "UTF8");
        out.beginResult("test");
        out.beginObject("1", Attribute.set("a", "b"))
        out.endObject()
        out.endResult()
        assertEquals("""{"1":{"a":"b"}}""",os.toString())
    }
}
