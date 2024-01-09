/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates

import com.google.common.collect.Lists
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.extension.ExtendWith
import sirius.kernel.BaseSpecification
import sirius.kernel.SiriusExtension
import sirius.web.data.LineBasedProcessor
import sirius.web.data.NamedRowProcessor
import sirius.web.data.SmartLineBasedProcessor
import sirius.web.data.SmartRow
import java.lang.Exception

import java.util.function.Predicate
import kotlin.reflect.KClass
import kotlin.test.assertEquals

@ExtendWith(SiriusExtension::class)
class SmartLineBasedProcessorTest {

    @Test
    fun `reading CSVs works with different column orders and aliases`() {
        
        val contents1 = Lists.newArrayList<SmartRow>()
        val contents2 = Lists.newArrayList<SmartRow>()
        val proc1 = SmartLineBasedProcessor()
            .withColumn("item", "artikel")
            .withColumn("quantity")
            .withProcessor { line: Int, row: SmartRow -> contents1.add(row) }
        val proc2 = SmartLineBasedProcessor()
            .withColumn("item", "artikel")
            .withColumn("quantity")
            .withProcessor { line: Int, row: SmartRow -> contents2.add(row) }

        val lineProc1 = LineBasedProcessor.create("smart-test1.csv", KClass::class.java.getResourceAsStream("/smart-test1.csv"))
        val lineProc2 = LineBasedProcessor.create("smart-test2.csv", KClass::class.java.getResourceAsStream("/smart-test2.csv"))

        lineProc1.run(proc1) { e: Exception? -> false }
        lineProc2.run(proc2) { e: Exception? -> false }

        assertEquals(2,contents1.size)
        assertEquals(2,contents2.size)

        assertEquals("A", contents1[0].getFirst("item").asString())
        assertEquals(1, contents1[0].getFirst("quantity").asInt(-1))

        assertEquals("A", contents2[0].getFirst("item").asString())
        assertEquals(1, contents2[0].getFirst("quantity").asInt(-1))

        assertEquals("B", contents1[1].getFirst("item").asString())
        assertEquals(2, contents1[1].getFirst("quantity").asInt(-1))

        assertEquals("B", contents2[1].getFirst("item").asString())
        assertEquals(2, contents2[1].getFirst("quantity").asInt(-1))
    }


}
