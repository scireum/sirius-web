/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.extension.ExtendWith
import sirius.kernel.SiriusExtension
import sirius.kernel.commons.Values
import sirius.web.data.LineBasedProcessor
import kotlin.reflect.KClass
import kotlin.test.assertEquals

@ExtendWith(SiriusExtension::class)
class LineBasedProcessorTest {

    @Test
    fun `readingExcel works including formulas`() {

        val processor = LineBasedProcessor.create("test.xls", KClass::class.java.getResourceAsStream("/test.xls"))
        val contents = ArrayList<Values>()

        processor.run({ _: Int, values: Values -> contents.add(values) }) { _ -> false }

        assertEquals(3, contents.size)
        assertEquals("A", contents[0].at("A").asString())
        assertEquals(2, contents[1].at("B").asInt(-1))
        assertEquals(6, contents[2].at("C").asInt(-1))
    }

    @Test
    fun `readingXSLX works including formulas`() {

        val processor = LineBasedProcessor.create("test.xlsx", KClass::class.java.getResourceAsStream("/test.xlsx"))
        val contents = ArrayList<Values>()

        processor.run({ _: Int, values: Values -> contents.add(values) }) { _ -> false }

        assertEquals(3, contents.size)
        assertEquals("A", contents[0].at("A").asString())
        assertEquals(2, contents[1].at("B").asInt(-1))
        assertEquals(6, contents[2].at("C").asInt(-1))
    }

    @Test
    fun `reading CSV works with line breaks`() {

        val processor = LineBasedProcessor.create("test.csv", KClass::class.java.getResourceAsStream("/test.csv"))
        val contents = ArrayList<Values>()

        processor.run({ _: Int, values: Values -> contents.add(values) }, { _ -> false })

        assertEquals(2, contents.size)
        assertEquals("A", contents[0].at("A").asString())
        assertEquals("Hallo Welt", contents[0].at("B").asString())
        assertEquals("Hallo\nWelt", contents[1].at("B").asString())
    }

}
