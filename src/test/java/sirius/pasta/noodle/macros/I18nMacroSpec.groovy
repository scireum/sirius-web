/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.macros

import sirius.kernel.BaseSpecification
import sirius.kernel.di.std.Part
import sirius.pasta.tagliatelle.Tagliatelle
import sirius.pasta.tagliatelle.compiler.TemplateCompilationContext
import sirius.pasta.tagliatelle.compiler.TemplateCompiler
import sirius.web.resources.Resources

class I18nMacroSpec extends BaseSpecification {

    @Part
    private static Tagliatelle tagliatelle

    @Part
    private static Resources resources

    def "basic tests"() {
        given:
        def ctx = tagliatelle.createInlineCompilationContext("inline", input,null)
        new TemplateCompiler(ctx).compile()
        expect:
        ctx.getTemplate().renderToString() == output
        where:
        input                                    | output
        "@i18n('I18nMacroSpec.test')"            | "test"
        "@i18n('I18nMacroSpec.multipleTest', 0)" | "first"
        "@i18n('I18nMacroSpec.multipleTest', 1)" | "second"
        "@i18n('I18nMacroSpec.multipleTest', 2)" | "third"
        "@i18n('', 2)"                           | ""
        "@i18n('')"                              | ""
    }
}
