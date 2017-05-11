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

    def "parser works"() {
        when:
        Compiler compiler = new Compiler(new StringReader("@if (false) {Hello W@'World'.substring(1)} else {hullo}!"), new CompilationContext())
        and:
        Template template = compiler.compile()
        and:
        RenderContext ctx = new RenderContext()
        then:
        template.render(ctx)
        ctx.buffer == 'Hello World!'

    }

}
