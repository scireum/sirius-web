/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates

import com.google.common.collect.Lists
import sirius.kernel.BaseSpecification
import sirius.web.data.LineBasedProcessor
import sirius.web.data.NamedRowProcessor
import sirius.web.data.SmartLineBasedProcessor
import sirius.web.data.SmartRow

class SmartLineBasedProcessorSpec extends BaseSpecification {

    def "reading CSVs works with different column orders and aliases"() {
        given:
        List<SmartRow> contents1 = Lists.newArrayList()
        List<SmartRow> contents2 = Lists.newArrayList()
        SmartLineBasedProcessor proc1 = new SmartLineBasedProcessor()
                .withColumn("item", "artikel")
                .withColumn("quantity")
                .withProcessor({ l, v -> contents1.add(v) } as NamedRowProcessor)
        SmartLineBasedProcessor proc2 = new SmartLineBasedProcessor()
                .withColumn("item", "artikel")
                .withColumn("quantity")
                .withProcessor({ l, v -> contents2.add(v) } as NamedRowProcessor)

        LineBasedProcessor lineProc1 = LineBasedProcessor.create("smart-test1.csv", getClass().getResourceAsStream("/smart-test1.csv"))
        LineBasedProcessor lineProc2 = LineBasedProcessor.create("smart-test2.csv", getClass().getResourceAsStream("/smart-test2.csv"))
        when:
        lineProc1.run(proc1)
        lineProc2.run(proc2)
        then:
        contents1.size() == 2
        contents2.size() == 2
        and:
        contents1.get(0).getFirst("item").asString() == 'A'
        contents1.get(0).getFirst("quantity").asInt(-1) == 1
        and:
        contents2.get(0).getFirst("item").asString() == 'A'
        contents2.get(0).getFirst("quantity").asInt(-1) == 1
        and:
        contents1.get(1).getFirst("item").asString() == 'B'
        contents1.get(1).getFirst("quantity").asInt(-1) == 2
        and:
        contents2.get(1).getFirst("item").asString() == 'B'
        contents2.get(1).getFirst("quantity").asInt(-1) == 2
    }


}
