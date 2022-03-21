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
import sirius.pasta.noodle.ScriptingException
import sirius.pasta.noodle.SimpleEnvironment

/**
 * Tests the Noodle parser and compiler.
 * <p>
 * Note that Tagliatelle also provides a large test set which tests assumptions on the compiler in
 * {@link sirius.pasta.tagliatelle.CompilerSpec}.
 */
class CompilerSpec extends BaseSpecification {

    Callable compile(String input) {
        def compilationContext = new CompilationContext(SourceCodeInfo.forInlineCode(input))
        Callable script = new NoodleCompiler(compilationContext.
                addImport(Position.UNKNOWN, "NoodleExample", NoodleExample.class)).
                compileScript()
        compilationContext.processCollectedErrors()

        return script
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
        and:
        compile("NoodleExample.A_DOUBLE + NoodleExample.AN_INT").call(new SimpleEnvironment()) == 4.2
        and:
        compile("NoodleExample.A_DOUBLE + NoodleExample.A_LONG").call(new SimpleEnvironment()) == 5.2
    }


    def "accessing fields works"() {
        expect:
        compile("NoodleExample.privateStaticField").call(new SimpleEnvironment()) == "Hello from the other side"
        and:
        compile("NoodleExample.privateStaticField = 'Hello'; return NoodleExample.privateStaticField;").
                call(new SimpleEnvironment()) == "Hello"
        and:
        compile("NoodleExample.INSTANCE.privateField").call(new SimpleEnvironment()) == "Hello World"
        and:
        compile("NoodleExample.INSTANCE.privateField = 'Hello'; return NoodleExample.INSTANCE.privateField;").
                call(new SimpleEnvironment()) == "Hello"
        and:
        compile("NoodleExample.filledOptional().orElse(null).privateField").
                call(new SimpleEnvironment()) == "Hello World"
    }

    def "parsing let/if/for works"() {
        expect:
        compile("let x = 5; if (3 < 4) { x = 3; } else { x = 4; }; return x;").call(new SimpleEnvironment()) == 3
        and: "Semicolon after closing brace can be skipped"
        compile("let x = 5; if (3 < 4) { x = 3; } else { x = 4; } return x;").call(new SimpleEnvironment()) == 3
        and:
        compile("let x = 3; x = 4; return x;").call(new SimpleEnvironment()) == 4
        and:
        compile("let sum = 0; for(int x : java.util.Arrays.asList(3, 4)) { sum = sum + x; }; return sum;").
                call(new SimpleEnvironment()) == 7
    }

    def "parsing lambdas works"() {
        expect: "Simple generic type propagation works"
        compile("let sum = 0; NoodleExample.intStream().forEach(|x| sum = sum + x); return sum;").
                call(new SimpleEnvironment()) == 6
        and: "Class derived generic type propagation works"
        compile("let sum = 0; NoodleExample.stream(java.lang.Integer.class).forEach(|x| sum = sum + x); return sum;").
                call(new SimpleEnvironment()) == 0
        and: "Object derived generic type propagation works"
        compile("let sum = 0; NoodleExample.singletonStream(42).forEach(|x| sum = sum + x); return sum;").
                call(new SimpleEnvironment()) == 42
        and: "Var-args generic type propagation works"
        compile("let sum = 0; java.util.Arrays.asList(3,4).forEach(|x| sum = sum + x); return sum;").
                call(new SimpleEnvironment()) == 7
        and: "zero-arg lambdas work"
        compile("let x = 0; NoodleExample.invokeUnitOfWork(|| x = 42); return x;").call(new SimpleEnvironment()) == 42
    }

    def "exceptions in lambdas work"() {
        when: "An undeclared exception is thrown within a lambda..."
        compile("NoodleExample.invokeConsumer(|x| { NoodleExample.throwDeclaredException(); })").call(new SimpleEnvironment())
        then: "The exception is turned into a ScriptingException if it is not throwable (undeclared) within a lambda..."
        thrown(ScriptingException)

        when: "A RuntimeException is thrown..."
        compile("NoodleExample.invokeConsumer(|x| { x / 0; })").call(new SimpleEnvironment())
        then: "The exception is re-thrown as it doesn't need to be declared..."
        thrown(ScriptingException)
    }

    def "conditions work as expected"() {
        expect:
        compile(input).call(new SimpleEnvironment()) == output
        where:
        input                                       | output
        "false"                                     | false
        "true"                                      | true
        "false && true"                             | false
        "true && true"                              | true
        "false || false"                            | false
        "false || true"                             | true
        "null.as(java.lang.Boolean.class) && true"  | false
        "null.as(java.lang.Boolean.class) && false" | false
        "null.as(java.lang.Boolean.class) || true"  | true
        "null.as(java.lang.Boolean.class) || false" | false
    }

    def "types can be derived from generic super classes"() {
        expect:
        compile("NoodleExample.longToString(NoodleExample.INSTANCE.getRef().getId())").call(new SimpleEnvironment()) == "42"
        and:
        compile("Strings.join(' ', NoodleExample.INSTANCE.getRef().getTest(), 'World')").call(new SimpleEnvironment()) == "Hello World"
    }

    def "incomplete class literals are detected and reported"() {
        when:
        def compilationContext = new CompilationContext(SourceCodeInfo.forInlineCode("part(sirius.pasta.tagliatelle.Tagliatelle).getExtensions(null)"))
        new NoodleCompiler(compilationContext).compileScript()
        then:
        compilationContext.getErrors().size() == 1
        and:
        compilationContext.getErrors().get(0).getMessage() == "  1: 6: Found an incomplete class literal 'class sirius.pasta.tagliatelle.Tagliatelle'. Add '.class' if you want to refer to the class object."
    }

    def "calling constructors works"() {
        expect: "calling a constructor works as expected..."
        compile("Tuple.new('A','B').getFirst()").call(new SimpleEnvironment()) == "A"
        and: "generic parameter propagation works as expected..."
        compile("Tuple.new('A', 1).getFirst().getClass().getName()").call(new SimpleEnvironment()) == "java.lang.String"
        compile("Tuple.new('A', 1).getSecond().getClass().getName()").call(new SimpleEnvironment()) == "java.lang.Integer"

    }
}
