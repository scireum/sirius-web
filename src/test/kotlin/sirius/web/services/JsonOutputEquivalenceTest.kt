/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.services

import org.junit.jupiter.api.Assertions.assertAll
import org.junit.jupiter.api.extension.ExtendWith
import sirius.kernel.SiriusExtension
import sirius.kernel.commons.Amount
import sirius.kernel.commons.Json
import sirius.kernel.commons.NumberFormat
import sirius.kernel.xml.JsonNodeStructuredOutput
import sirius.kernel.xml.StructuredOutput
import java.io.StringWriter
import java.time.LocalDateTime
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertNotEquals

/**
 * Pins that building a payload in memory yields exactly what streaming it would have produced.
 *
 * [JsonNodeStructuredOutput] exists so that code written to describe a payload can also be used where the payload is
 * needed as a value, and it says of itself that values are rendered as the streaming output renders them. That is a
 * promise about [JSONStructuredOutput], made from another module — so this is the only place it can be kept honest:
 * sirius-kernel cannot see this class, and nothing downstream should have to check the framework's own claim.
 *
 * Every kind of value a service actually writes is therefore written through both and compared as text.
 */
@ExtendWith(SiriusExtension::class)
class JsonOutputEquivalenceTest {

    @Test
    fun `a flat object serialises identically`() {
        assertEquals(streamed { it.property("name", "Zuul").property("count", 3) },
                     built { it.property("name", "Zuul").property("count", 3) })
    }

    @Test
    fun `every kind of value a service writes serialises identically`() {
        val timestamp = LocalDateTime.of(2026, 7, 31, 14, 30, 5)
        val payload: (StructuredOutput) -> Unit = {
            it.property("text", "hello")
                .property("nothing", null)
                .property("yes", true)
                .property("no", false)
                .property("int", 42)
                .property("long", 9_000_000_000L)
                .property("double", 1.5)
                .property("timestamp", timestamp)
                .property("amount", Amount.of(12.5))
        }

        assertEquals(streamed(payload), built(payload))
    }

    @Test
    fun `nesting and arrays serialise identically`() {
        val payload: (StructuredOutput) -> Unit = {
            it.property("id", "RAJ4MS171IUC9H07OEBR9NSRT3")
            it.beginObject("sender")
            it.property("name", "Egon")
            it.endObject()
            it.beginArray("references")
            it.beginObject("reference")
            it.property("value", "RG1234567")
            it.endObject()
            it.beginObject("reference")
            it.property("value", "RG7654321")
            it.endObject()
            it.endArray()
            it.beginArray("empty")
            it.endArray()
            it.property("resultsOmitted", false)
        }

        assertEquals(streamed(payload), built(payload))
    }

    @Test
    fun `strings needing escapes serialise identically`() {
        // one escapes by hand while the other leaves it to Jackson, so this is where they would drift apart first
        val payload: (StructuredOutput) -> Unit = {
            it.property("quote", "she said \"zuul\"")
                .property("backslash", "C:\\temp\\file")
                .property("newline", "line1\nline2")
                .property("tab", "a\tb")
                .property("unicode", "Grüße, 世界")
                .property("control", "bell\u0007end")
        }

        assertEquals(streamed(payload), built(payload))
    }

    @Test
    fun `an embedded json node is carried through unchanged`() {
        val embedded = Json.createObject().put("nested", "value")
        val payload: (StructuredOutput) -> Unit = { it.property("payload", embedded) }

        assertAll(
            { assertEquals(streamed(payload), built(payload)) },
            { assertEquals("""{"payload":{"nested":"value"}}""", built(payload)) }
        )
    }

    @Test
    fun `a machine-formatted amount is a number in both`() {
        val payload: (StructuredOutput) -> Unit = {
            it.amountProperty("price", Amount.of(1234.5), NumberFormat.MACHINE_TWO_DECIMAL_PLACES, false)
        }

        assertEquals(streamed(payload), built(payload))
    }

    @Test
    fun `a localized amount is where the two deliberately part company`() {
        // amountProperty hands on whatever the NumberFormat produced, and the two do different things with it:
        // streaming writes the formatted text into the document unquoted, so a format carrying a grouping separator
        // leaves the document unparseable, while building a node keeps it as a string. Pinned rather than left to be
        // discovered: the equivalence above holds for everything a service should be writing, and this is the one
        // exception. The expected text is taken from the formatter itself, so this says the same thing in any locale.
        val price = Amount.of(1234.5)
        val formatted = price.toString(NumberFormat.TWO_DECIMAL_PLACES).asString()
        val payload: (StructuredOutput) -> Unit = {
            it.amountProperty("price", price, NumberFormat.TWO_DECIMAL_PLACES, false)
        }

        val streamed = streamed(payload)
        val built = built(payload)

        assertAll(
            { assertEquals("""{"price":$formatted}""", streamed, "streamed unquoted, verbatim as formatted") },
            { assertEquals("""{"price":"$formatted"}""", built, "kept as a string, so the document stays parseable") },
            { assertNotEquals(price.toString(NumberFormat.MACHINE_TWO_DECIMAL_PLACES).asString(), formatted,
                              "a localized format that equals the machine one would make this case prove nothing") }
        )
    }

    /** Writes the payload the way a service does, straight into the response, and returns the JSON text. */
    private fun streamed(payload: (StructuredOutput) -> Unit): String {
        val writer = StringWriter()
        val output = JSONStructuredOutput(writer)
        output.beginResult()
        payload(output)
        output.endResult()
        return writer.toString()
    }

    /** Writes the same payload into a node and serialises that. */
    private fun built(payload: (StructuredOutput) -> Unit): String {
        val output = JsonNodeStructuredOutput()
        output.beginResult()
        payload(output)
        output.endResult()
        return Json.write(output.node)
    }
}
