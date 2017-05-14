/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates.engine

import sirius.kernel.BaseSpecification

/**
 * Created by aha on 11.05.17.
 */
class CompilerSpec extends BaseSpecification {

    //TODO automatic attributes
    //INVOKE TAG / Arguments -> Template Stack mit Basepointer
    //RuntimeErrors
    //Static Method Calls
    //Engine / Environment 1 per Scope
    //Pragma / inline
    // $ for i18n
    // Smart Escaping
    // Caching und co.

    def "parser works"() {
        when:
        CompilationContext cc = new CompilationContext()
        Compiler compiler = new Compiler('<i:if test="@3<4">A<i:block name="else">B</i:block></i:if>', cc)
        and:
        Template template = compiler.compile()
        println cc
        and:
        RenderContext ctx = new RenderContext()
        then:
        template.render(ctx)
        ctx.buffer == 'Hello World!'

    }

}
