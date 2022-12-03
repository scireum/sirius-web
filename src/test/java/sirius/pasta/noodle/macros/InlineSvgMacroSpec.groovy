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
import sirius.pasta.tagliatelle.compiler.TemplateCompiler
import sirius.web.resources.Resources

class InlineSvgMacroSpec extends BaseSpecification {

    @Part
    private static Tagliatelle tagliatelle

    @Part
    private static Resources resources

    def "inlineSVG excludes the XML declaration"() {
        when:
        def ctx = tagliatelle.createInlineCompilationContext("inline", "@inlineSVG('/assets/test.svg')", null)
        new TemplateCompiler(ctx).compile()
        then:
        ctx
                .getTemplate()
                .renderToString() == "<svg xmlns=\"http://www.w3.org/2000/svg\" height=\"32\" version=\"1.1\" viewBox=\"0 0 32 32\" width=\"32\">\n" +
                "    <path d=\"m26 7c1.1046 0 2 0.89543 2 2v15c0 1.1046-0.89543 2-2 2h-19c-1.1046 0-2-0.89543-2-2v-15c0-1.1046 0.89543-2 2-2h19zm0 2h-19v15h19v-15zm-12.2 3c0.1704 0 0.32 0.0615 0.4504 0.147l6.2168 3.654c0.3088 0.10275 0.5328 0.3735 0.5328 0.699 0 0.24375-0.132 0.45075-0.324 0.58725l-6.3792 3.7365c-0.1376 0.10425-0.3064 0.17625-0.4968 0.17625-0.4424 0-0.8-0.33525-0.8-0.75v-7.5c0-0.414 0.3576-0.75 0.8-0.75z\"/>\n" +
                "</svg>"
    }
}
