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

class XmlMacroSpec extends BaseSpecification {

    @Part
    private static Tagliatelle tagliatelle

    @Part
    private static Resources resources

    def "xml correctly parses stringified XML"() {
        when:
        def ctx = tagliatelle.createInlineCompilationContext("inline",
                                                             "@xml('<a attrib=\"wert\"><b name=\"wert\" attrib=\"payload\" /></a>')",
                                                             null)
        new TemplateCompiler(ctx).compile()
        then:
        ctx
                .getTemplate()
                .renderToString() == "<a attrib=\"wert\">\n" +
                "    <b attrib=\"payload\" name=\"wert\"/>\n" +
                "</a>\n"
    }
}
