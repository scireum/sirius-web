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
 * Tests the [FormatNumberMacro].
 */
@ExtendWith(SiriusExtension::class)
class FormatNumberMacroTest {

    @ParameterizedTest
    @CsvSource(
        delimiter = '|', useHeadersInDisplayName = true, textBlock = // language=CSV
            """input                                         | output
            '@formatNumber(3005)'                          | 3.005
            '@formatNumber(40459685)'                      | 40.459.685
            '@formatNumber(0.84)'                          | 0,84
            '@formatNumber(4289.333)'                      | 4.289,33
            '@formatNumber(Amount.of(1234567))'            | 1.234.567
            '@formatNumber(Amount.NOTHING)'                | ''
            '@formatNumber(null)'                          | ''"""
    )
    fun `Basic scenarios of the macro work as expected`(input: String, output: String) {
        val context = tagliatelle.createInlineCompilationContext("inline", input, SandboxMode.DISABLED, null)
        TemplateCompiler(context).compile()
        assertEquals(output, context.template.renderToString())
    }

    @Test
    fun `Unsupported parameter types are rejected at compile time`() {
        val context =
            tagliatelle.createInlineCompilationContext("inline", "@formatNumber(\"42\")", SandboxMode.DISABLED, null)
        assertFailsWith<CompileException> { TemplateCompiler(context).compile() }
    }

    companion object {
        @JvmStatic
        @Part
        private lateinit var tagliatelle: Tagliatelle
    }
}
