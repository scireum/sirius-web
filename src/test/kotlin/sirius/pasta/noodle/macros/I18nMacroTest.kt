/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.macros

import org.junit.jupiter.api.extension.ExtendWith
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.CsvSource
import sirius.kernel.SiriusExtension
import sirius.kernel.di.std.Part
import sirius.pasta.tagliatelle.Tagliatelle
import sirius.pasta.tagliatelle.compiler.TemplateCompiler
import sirius.web.resources.Resources
import kotlin.test.assertEquals

/**
 * Tests the [I18nMacro].
 */
@ExtendWith(SiriusExtension::class)
class I18nMacroTest {

    @ParameterizedTest
    @CsvSource(
            delimiter = '|', useHeadersInDisplayName = true, textBlock = """
         input                                      | output
        '@i18n("I18nMacroSpec.test")'               | test
        '@i18n("I18nMacroSpec.multipleTest", 0)'    | first
        '@i18n("I18nMacroSpec.multipleTest", 1)'    | second
        '@i18n("I18nMacroSpec.multipleTest", 2)'    | third
        '@i18n("", 2)'                              | ''
        '@i18n("")'                                 | ''"""
    )
    fun `Basic scenarios of the macro work as expected`(input: String, output: String) {
        val context = tagliatelle.createInlineCompilationContext("inline", input, null)
        TemplateCompiler(context).compile()
        assertEquals(output, context.template.renderToString())
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
