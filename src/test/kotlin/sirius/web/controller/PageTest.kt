/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */


package sirius.web.controller

import spock.lang.Specification
import java.awt.Stroke
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse

class PageTest {

    @Test
    fun `linkToPageWithConfigurableStart() returns 'start=' if no filters are set`() {
        val page: Page<String> = Page()
        val result = page.linkToPageWithConfigurableStart("/test")
        assertEquals("/test?start=", result)
    }

    @Test
    fun `linkToCurrentPage() works with an URI that already contains a query string`() {
        val page: Page<String> = Page()
        page.withQuery("QUERY").withFacets(
            listOf(
                Facet("", "field1", "value1", null),
                Facet("", "field2", "value2", null)
            )
        )
        val result = page.linkToCurrentPage("/test?hallo=welt")
        assertEquals("/test?hallo=welt&query=QUERY&field1=value1&field2=value2", result)
    }

    @Test
    fun `linkToPageWithConfigurableStart() returns a valid query string ending with start=`() {
        val page: Page<String> = Page()
        page.withQuery("QUERY").withFacets(
            listOf(
                Facet("", "field1", "value1", null),
                Facet("", "field2", "value2", null)
            )
        )
        val result = page.linkToPageWithConfigurableStart("/test")
        result.contains("field1=value1&")
        result.contains("field2=value2&")
        result.contains("query=QUERY&")
        result.endsWith("&start=")
    }

    @Test
    fun `withLimitedItemsSupplier() removes elements which exceed the page size`() {
        val limitPage = Page<String>().withPageSize(5)
        val elementsList = ArrayList<String>()
        elementsList.add("1")
        elementsList.add("2")
        elementsList.add("3")
        elementsList.add("4")
        elementsList.add("5")
        elementsList.add("6")
        elementsList.add("7")
        limitPage.withLimitedItemsSupplier { _ -> elementsList }
        assertEquals(5, limitPage.getItems().size)
        assertEquals(true, limitPage.hasMore())
        assertEquals("5", limitPage.getItems().get(4))
        assertEquals("1", limitPage.getItems().get(0))
    }

    @Test
    fun `withLimitedItemsSupplier() with a list smaller than the page size does not crash`() {
        val limitPage = Page<String>().withPageSize(5)
        val elementsList = ArrayList<String>()
        elementsList.add("1")
        elementsList.add("2")
        elementsList.add("3")
        limitPage.withLimitedItemsSupplier { _ -> elementsList }
        assertEquals(3, limitPage.getItems().size)
        assertFalse(limitPage.hasMore())
        assertEquals("3", limitPage.getItems().get(2))
        assertEquals("1", limitPage.getItems().get(0))
    }

}
