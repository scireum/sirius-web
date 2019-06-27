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


    def "linkToPageWithConfigurableStart() returns 'start=' if no filters are set"() {
        given:
        Page page = new Page()
        when:
        def result = page.linkToPageWithConfigurableStart('/test')
        then:
        result == "/test?start="
    }

    def "linkToCurrentPage() works with an URI that already contains a query string"() {
        given:
        Page page = new Page()
        when:
        page.withQuery("QUERY").
                withFactes([
                        new Facet("", "field1", "value1", null),
                        new Facet("", "field2", "value2", null)
                ])
        and:
        def result = page.linkToCurrentPage('/test?hallo=welt')
        then:
        result == '/test?hallo=welt&query=QUERY&field1=value1&field2=value2'
    }

    def "linkToPageWithConfigurableStart() returns a valid query string ending with start="() {
        given:
        Page page = new Page()
        when:
        page.withQuery("QUERY").
                withFactes([
                        new Facet("", "field1", "value1", null),
                        new Facet("", "field2", "value2", null)
                ])
        and:
        def result = page.linkToPageWithConfigurableStart('/test')
        then:
        result.contains("field1=value1&")
        result.contains("field2=value2&")
        result.contains("query=QUERY&")
        result.endsWith("&start=")
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
