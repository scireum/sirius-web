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


    def "createQueryStringForConfigurableStart() returns a valid query string ending with start="() {
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

    def "withLimitedItemsSupplier() removes elements which exceed the page size"() {
        given:
        def limitPage = new Page<>().withPageSize(5)
        def elementsList = new ArrayList()
        elementsList.add("1")
        elementsList.add("2")
        elementsList.add("3")
        elementsList.add("4")
        elementsList.add("5")
        elementsList.add("6")
        elementsList.add("7")
        when:
        limitPage.withLimitedItemsSupplier{limit -> elementsList}
        then:
        limitPage.getItems().size() == 5
        and:
        limitPage.hasMore() == true
        and:
        limitPage.getItems().get(4) == "5"
        limitPage.getItems().get(0) == "1"
    }

    def "withLimitedItemsSupplier() with a list smaller than the page size does not crash"() {
        given:
        def limitPage = new Page<>().withPageSize(5)
        def elementsList = new ArrayList()
        elementsList.add("1")
        elementsList.add("2")
        elementsList.add("3")
        when:
        limitPage.withLimitedItemsSupplier{limit -> elementsList}
        then:
        limitPage.getItems().size() == 3
        and:
        limitPage.hasMore() == false
        and:
        limitPage.getItems().get(2) == "3"
        limitPage.getItems().get(0) == "1"
    }

}
