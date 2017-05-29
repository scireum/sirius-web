/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle

import sirius.kernel.BaseSpecification
import sirius.kernel.di.std.Part
import sirius.tagliatelle.compiler.CompilationContext
import sirius.tagliatelle.compiler.Compiler
import sirius.tagliatelle.rendering.LocalRenderContext

import java.util.function.Consumer

/**
 * Created by aha on 11.05.17.
 */
class CompilerSpec extends BaseSpecification {

    //TODO
    // Protect against loops
    //Pragma / inline
    // Smart Escaping
    // Runtime-Mode (friendly, hard-crash, fail-safe)
    // Über pragma steuerbar

    @Part
    private static Engine engine

    def "parser works"() {
        when:
        CompilationContext cc = engine.createCompilationContext(path, resource, parentContext);
        Compiler compiler = new Compiler("test", null,'<w:test1><i:block name="breadcrumbs">A</i:block></w:test1>', cc)
        and:
        Template template = compiler.compile()
        println cc
        and:
        StringBuilder sb = new StringBuilder()
        LocalRenderContext ctx = engine.createRenderContext(new Consumer<String>() {
            @Override
            void accept(String s) {
                sb.append(s)
            }
        }).createContext(template);
        then:
        template.render(ctx)
        sb.toString() == 'Hello World!'

    }

}
