/*
 * Made with all the love in the world
 * by scireum in Stuttgart, Germany
 *
 * Copyright by scireum GmbH
 * https://www.scireum.de - info@scireum.de
 */

@file:Suppress("DANGEROUS_CHARACTERS")

package sirius.pasta.noodle.macros

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.extension.ExtendWith
import sirius.kernel.SiriusExtension
import sirius.kernel.di.std.Part
import sirius.pasta.noodle.sandbox.SandboxMode
import sirius.pasta.tagliatelle.Tagliatelle
import sirius.pasta.tagliatelle.compiler.TemplateCompiler
import kotlin.test.assertEquals

/**
 * Tests the [RenderMarkdownMacro].
 */
@ExtendWith(SiriusExtension::class)
class RenderMarkdownMacroTest {

    private fun render(markdown: String): String {
        val context = tagliatelle.createInlineCompilationContext(
            "inline",
            "<i:arg type=\"String\" name=\"text\"/><i:raw>@renderMarkdown(text)</i:raw>",
            SandboxMode.DISABLED,
            null
        )
        TemplateCompiler(context).compile()
        return context.template.renderToString(markdown)
    }

    @Test
    fun `renders lists and inline code`() {
        assertEquals(
            "<p>The request is invalid:</p>\n"
                    + "<ul>\n<li><code>invalid_quantity</code>: the quantity is not positive</li>\n</ul>",
            render("The request is invalid:\n- `invalid_quantity`: the quantity is not positive")
        )
    }

    @Test
    fun `escapes HTML contained in the text`() {
        assertEquals(
            "<p>Text &lt;script&gt;alert(1)&lt;/script&gt; <a rel=\"nofollow\" href=\"\">link</a></p>",
            render("Text <script>alert(1)</script> [link](javascript:void)")
        )
    }

    @Test
    fun `renders an empty text as empty string`() {
        assertEquals("", render(""))
    }

    companion object {
        @JvmStatic
        @Part
        private lateinit var tagliatelle: Tagliatelle
    }
}
