/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates

import sirius.kernel.BaseSpecification
import sirius.kernel.commons.Values
import sirius.web.data.LineBasedProcessor
import sirius.web.data.RowProcessor

import java.util.function.Predicate

class LineBasedProcessorSpec extends BaseSpecification {

    def "readingExcel works including formulas"() {
        given:
        LineBasedProcessor proc = LineBasedProcessor.create("test.xls", getClass().getResourceAsStream("/test.xls"))
        List<Values> contents = new ArrayList<>()
        when:
        proc.run({ l, v -> contents.add(v) } as RowProcessor, {e -> false} as Predicate)
        then:
        contents.size() == 3
        and:
        contents.get(0).at("A").asString() == 'A'
        and:
        contents.get(1).at("B").asInt(-1) == 2
        and:
        contents.get(2).at("C").asInt(-1) == 6
    }

    def "readingXSLX works including formulas"() {
        given:
        LineBasedProcessor proc = LineBasedProcessor.create("test.xlsx", getClass().getResourceAsStream("/test.xlsx"))
        List<Values> contents = new ArrayList<>()
        when:
        proc.run({ l, v -> contents.add(v) } as RowProcessor, {e -> false} as Predicate)
        then:
        contents.size() == 3
        and:
        contents.get(0).at("A").asString() == 'A'
        and:
        contents.get(1).at("B").asInt(-1) == 2
        and:
        contents.get(2).at("C").asInt(-1) == 6
    }

    def "reading CSV works with line breaks"() {
        given:
        LineBasedProcessor proc = LineBasedProcessor.create("test.csv", getClass().getResourceAsStream("/test.csv"))
        List<Values> contents =  new ArrayList<>()
        when:
        proc.run({ l, v -> contents.add(v) } as RowProcessor, {e -> false} as Predicate)
        then:
        contents.size() == 2
        and:
        contents.get(0).at("A").asString() == 'A'
        and:
        contents.get(0).at("B").asString() == 'Hallo Welt'
        and:
        contents.get(1).at("B").asString() == 'Hallo\nWelt'
    }

}
