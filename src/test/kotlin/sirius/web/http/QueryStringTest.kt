/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.http

import org.junit.jupiter.api.Test
import sirius.kernel.commons.Value
import java.net.URI
import kotlin.test.*

class QueryStringTest {

    companion object {
        private const val TEST_URI = "/test?param1=value1&param2=value1&param2=value2&param3=&param4"
    }

    @Test
    fun `parses URI string correctly`() {
        val queryString = QueryString(TEST_URI)
        assertEquals("/test", queryString.path())
        assertEquals(setOf("param1", "param2", "param3", "param4"), queryString.parameterNames)
    }

    @Test
    fun `parses URI correctly`() {
        val queryString: QueryString = QueryString(URI.create(TEST_URI))
        assertEquals("/test", queryString.path())
        assertEquals(setOf("param1", "param2", "param3", "param4"), queryString.parameterNames)
    }

    @Test
    fun `'get' handles parameters as expected`() {
        val queryString = QueryString(TEST_URI)

        // Parameter with a single value is wrapped in Value
        assertEquals(Value.of("value1"), queryString.get("param1"))

        // Parameter with multiple values is wrapped as a list in Value
        assertEquals(Value.of(listOf("value1", "value2")), queryString.get("param2"))

        // Parameter with empty value is represented as empty Value
        assertTrue { queryString.get("param3").isEmptyString }

        // Parameter without value is represented as empty Value
        assertTrue { queryString.get("param4").isEmptyString }

        // Non-existing parameter is represented as empty Value
        assertTrue { queryString.get("param5").isEmptyString }
    }

    @Test
    fun `'getParameter' handles parameters as expected`() {
        val queryString = QueryString(TEST_URI)

        // Parameter with a single value is given as is
        assertEquals("value1", queryString.getParameter("param1"))

        // Parameter with multiple values gives the first value
        assertEquals("value1", queryString.getParameter("param2"))

        // Parameter with empty value is represented as empty string
        assertEquals("", queryString.getParameter("param3"))

        // Parameter without value is represented as empty string
        assertEquals("", queryString.getParameter("param4"))

        // Non-existing parameter is represented as null
        assertNull(queryString.getParameter("param5"))
    }

    @Test
    fun `'getParameters' handles parameters as expected`() {
        val queryString = QueryString(TEST_URI)

        // Parameter with a single value is given as a list of values
        queryString.getParameters("param1").apply {
            assertNotNull(this)
            assertEquals(1, this.size)
            assertTrue { this.contains("value1") }
        }

        // Parameter with multiple values is given as a list of values
        queryString.getParameters("param2").apply {
            assertNotNull(this)
            assertEquals(2, this.size)
            assertTrue { this.contains("value1") && this.contains("value2") }
        }

        // Parameter with empty value is represented as a list with a single empty string
        queryString.getParameters("param3").apply {
            assertNotNull(this)
            assertEquals(1, this.size)
            assertTrue { this.contains("") }
        }

        // Parameter without value is represented as a list with a single empty string
        queryString.getParameters("param4").apply {
            assertNotNull(this)
            assertEquals(1, this.size)
            assertTrue { this.contains("") }
        }

        // Non-existing parameter is represented as an empty list
        queryString.getParameters("param5").apply {
            assertNotNull(this)
            assertTrue { this.isEmpty() }
        }
    }

    @Test
    fun `'hasParameter' handles parameters as expected`() {
        val queryString = QueryString(TEST_URI)

        // Parameter with a single value is flagged as present
        assertTrue { queryString.hasParameter("param1") }

        // Parameter with multiple values is flagged as present
        assertTrue { queryString.hasParameter("param2") }

        // Parameter with empty value is flagged as present
        assertTrue { queryString.hasParameter("param3") }

        // Parameter without value is flagged as present
        assertTrue { queryString.hasParameter("param4") }

        // Non-existing parameter is flagged as missing
        assertFalse { queryString.hasParameter("param5") }
    }
}
