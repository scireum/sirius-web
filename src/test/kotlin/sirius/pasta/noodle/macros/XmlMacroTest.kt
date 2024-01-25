/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.macros

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.extension.ExtendWith
import sirius.kernel.SiriusExtension
import sirius.kernel.di.std.Part
import sirius.pasta.tagliatelle.Tagliatelle
import sirius.pasta.tagliatelle.compiler.TemplateCompiler
import sirius.web.resources.Resources
import kotlin.test.assertEquals

/**
 * Tests the [XmlMacro].
 */
@ExtendWith(SiriusExtension::class)
class XmlMacroTest {

    @Test
    fun `XmlMacro correctly parses stringified XML`() {
        val context = tagliatelle.createInlineCompilationContext(
                "inline",
                "@xml('<a attrib=\"wert\"><b name=\"wert\" attrib=\"payload\" /></a>')",
                null
        )
        TemplateCompiler(context).compile()
        assertEquals(
                "<a attrib=\"wert\">\n" +
                        "    <b attrib=\"payload\" name=\"wert\"/>\n" +
                        "</a>\n", context
                .template
                .renderToString()
        )
    }

    companion object {
        @JvmStatic
        @Part
        private lateinit var tagliatelle: Tagliatelle

        @JvmStatic
        @Part
        private lateinit var resources: Resources
    }
}
