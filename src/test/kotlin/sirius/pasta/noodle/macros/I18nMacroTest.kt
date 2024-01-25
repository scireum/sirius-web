/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.macros

class I18nMacroTest extends BaseSpecification {

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
