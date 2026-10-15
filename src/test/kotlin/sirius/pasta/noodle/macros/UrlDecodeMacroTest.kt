/*
 * Made with all the love in the world
 * by scireum in Stuttgart, Germany
 *
 * Copyright by scireum GmbH
 * https://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.macros

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.extension.ExtendWith
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.CsvSource
import sirius.kernel.SiriusExtension
import sirius.kernel.di.std.Part
import sirius.pasta.noodle.compiler.CompileException
import sirius.pasta.noodle.sandbox.SandboxMode
import sirius.pasta.tagliatelle.Tagliatelle
import sirius.pasta.tagliatelle.compiler.TemplateCompiler
import kotlin.test.assertEquals
import kotlin.test.assertFailsWith

/**
 * Tests the [UrlDecodeMacro].
 */
@ExtendWith(SiriusExtension::class)
class UrlDecodeMacroTest {

    @ParameterizedTest
    @CsvSource(
        delimiter = '|', useHeadersInDisplayName = true, textBlock = // language=CSV
            """input                                         | output
            '@urlDecode("%2Fwork%2FA%26B")'              | /work/A&B
            '@urlDecode("A+B%20C")'                      | A B C
            '@urlDecode("plain")'                        | plain
            '@urlDecode(urlEncode("/a b&c?d=e"))'        | /a b&c?d=e
            '@urlDecode("")'                             | ''
            '@urlDecode(null)'                           | ''"""
    )
    fun `Basic scenarios of the macro work as expected`(input: String, output: String) {
        val context = tagliatelle.createInlineCompilationContext("inline", input, SandboxMode.DISABLED, null)
        TemplateCompiler(context).compile()
        assertEquals(output, context.template.renderToString())
    }

    @Test
    fun `Unsupported parameter types are rejected at compile time`() {
        val context =
            tagliatelle.createInlineCompilationContext("inline", "@urlDecode(42)", SandboxMode.DISABLED, null)
        assertFailsWith<CompileException> { TemplateCompiler(context).compile() }
    }

    companion object {
        @JvmStatic
        @Part
        private lateinit var tagliatelle: Tagliatelle
    }
}
