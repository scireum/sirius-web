/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.compiler


import parsii.tokenizer.Position
import sirius.kernel.BaseSpecification
import sirius.pasta.noodle.Callable
import sirius.pasta.noodle.SimpleEnvironment

class CompilerSpec extends BaseSpecification {

    Callable compile(String input) {
        def compilationContext = new CompilationContext(SourceCodeInfo.forInlineCode(input))
        return new NoodleCompiler(compilationContext.addImport(Position.UNKNOWN, "NoodleExample", NoodleExample.class)).
                compileScript()
    }

    def "parsing and coercing literals works"() {
        expect:
        compile("NoodleExample.intToString(NoodleExample.AN_INT)").call(new SimpleEnvironment()) == "3"
        and:
        compile("NoodleExample.intToString(NoodleExample.AN_INTEGER_OBJECT)").call(new SimpleEnvironment()) == "12"
        and:
        compile("NoodleExample.longToString(NoodleExample.A_LONG)").call(new SimpleEnvironment()) == "4"
        and:
        compile("NoodleExample.longToString(NoodleExample.A_LONG_OBJECT)").call(new SimpleEnvironment()) == "33"
        and:
        compile("NoodleExample.longToString(NoodleExample.AN_INT)").call(new SimpleEnvironment()) == "3"
        and:
        compile("NoodleExample.AN_INT + NoodleExample.A_LONG_OBJECT").call(new SimpleEnvironment()) == 36L
    }

}
