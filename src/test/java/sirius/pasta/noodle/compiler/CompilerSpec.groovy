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

    def "accessing fields works"() {
        expect:
        compile("NoodleExample.privateStaticField").call(new SimpleEnvironment()) == "Hello from the other side"
        and:
        compile("NoodleExample.privateStaticField = 'Hello'; return NoodleExample.privateStaticField;").call(new SimpleEnvironment()) == "Hello"
        and:
        compile("NoodleExample.INSTANCE.privateField").call(new SimpleEnvironment()) == "Hello World"
        and:
        compile("NoodleExample.INSTANCE.privateField = 'Hello'; return NoodleExample.INSTANCE.privateField;").call(new SimpleEnvironment()) == "Hello"
    }

    def "conditions work as expected"() {
        expect:
        compile(input).call(new SimpleEnvironment()) == output
        where:
        input            | output
        "false"          | false
        "true"           | true
        "false && true"  | false
        "true && true"   | true
        "false || false" | false
        "false || true"  | true
        "null && true"   | false
        "null && false"  | false
        "null || true"   | true
        "null || false"  | false
    }
}
