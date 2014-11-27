/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */


package sirius.web.controller

import spock.lang.Specification

class PageSpec extends Specification {

    Page page = new Page()

    def "createQueryStringForConfigurableStart() returns 'start=' if no filters are set"() {
        when:
        def result = page.createQueryStringForConfigurableStart()
        then:
        result == "start="
    }


    def """createQueryStringForConfigurableStart() returns a query which
             - contains all specified parameters and
             - ends with 'start='"""() {
        given:
        page.withQuery("QUERY").
                withFactes([
                        facet("field1", "value1"),
                        facet("field2", "value2")
                ])
        when:
        def result = page.createQueryStringForConfigurableStart()
        then:
        result.contains("field1=value1&")
        result.contains("field2=value2&")
        result.contains("query=QUERY&")
        result.endsWith("&start=")
    }

    static Facet facet(String field, String value) {
        return new Facet("", field, value, null)
    }

}
