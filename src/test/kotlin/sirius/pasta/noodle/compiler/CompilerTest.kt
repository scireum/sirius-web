/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.compiler

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.assertThrows
import org.junit.jupiter.api.extension.ExtendWith
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.CsvSource
import sirius.kernel.SiriusExtension
import sirius.kernel.tokenizer.Position
import sirius.pasta.noodle.Callable
import sirius.pasta.noodle.ScriptingException
import sirius.pasta.noodle.SimpleEnvironment
import kotlin.test.assertEquals


/**
 * Tests the Noodle parser and compiler.
 * <p>
 * Note that Tagliatelle also provides a large test set which tests assumptions on the compiler in
 * {@link sirius.pasta.tagliatelle.CompilerSpec}.
 */
@ExtendWith(SiriusExtension::class)
class CompilerTest {

    private fun compile(input: String): Callable {
        val compilationContext = CompilationContext(SourceCodeInfo.forInlineCode(input))
        val script = NoodleCompiler(
                compilationContext.addImport(Position.UNKNOWN, "NoodleExample", NoodleExample::class.java)
        ).compileScript()
        compilationContext.processCollectedErrors()

        return script
    }

    @Test
    fun `Parsing and coercing literals works`() {
        assertEquals("3", compile("NoodleExample.intToString(NoodleExample.AN_INT)").call(SimpleEnvironment()))
        assertEquals(
                "12", compile("NoodleExample.intToString(NoodleExample.AN_INTEGER_OBJECT)").call(SimpleEnvironment())
        )
        assertEquals(
                "4", compile("NoodleExample.longToString(NoodleExample.A_LONG)").call(SimpleEnvironment())
        )
        assertEquals(
                "33", compile("NoodleExample.longToString(NoodleExample.A_LONG_OBJECT)").call(SimpleEnvironment())
        )
        assertEquals(
                "3", compile("NoodleExample.longToString(NoodleExample.AN_INT)").call(SimpleEnvironment())
        )
        assertEquals(
                36L, compile("NoodleExample.AN_INT + NoodleExample.A_LONG_OBJECT").call(SimpleEnvironment())
        )
        assertEquals(
                4.2, compile("NoodleExample.A_DOUBLE + NoodleExample.AN_INT").call(SimpleEnvironment())
        )
        assertEquals(
                5.2, compile("NoodleExample.A_DOUBLE + NoodleExample.A_LONG").call(SimpleEnvironment())
        )
    }

    @Test
    fun `Accessing fields works`() {
        assertEquals(
                "Hello from the other side", compile("NoodleExample.privateStaticField").call(SimpleEnvironment())
        )
        assertEquals(
                "Hello",
                compile("NoodleExample.privateStaticField = 'Hello'; return NoodleExample.privateStaticField;").call(
                        SimpleEnvironment()
                )
        )
        assertEquals(
                "Hello World", compile("NoodleExample.INSTANCE.privateField").call(SimpleEnvironment())
        )
        assertEquals(
                "Hello",
                compile("NoodleExample.INSTANCE.privateField = 'Hello'; return NoodleExample.INSTANCE.privateField;").call(
                        SimpleEnvironment()
                )
        )
        assertEquals(
                "Hello World",
                compile("NoodleExample.filledOptional().orElse(null).privateField").call(SimpleEnvironment())
        )
    }

    @Test
    fun `Parsing let,if,for works`() {
        assertEquals(
                3, compile("let x = 5; if (3 < 4) { x = 3; } else { x = 4; }; return x;").call(SimpleEnvironment())
        )
        // Semicolon after closing brace can be skipped
        assertEquals(
                3, compile("let x = 5; if (3 < 4) { x = 3; } else { x = 4; } return x;").call(SimpleEnvironment())
        )
        assertEquals(
                4, compile("let x = 3; x = 4; return x;").call(SimpleEnvironment())
        )
        assertEquals(
                7,
                compile("let sum = 0; for(int x : java.util.Arrays.asList(3, 4)) { sum = sum + x; }; return sum;").call(
                        SimpleEnvironment()
                )
        )
    }

    @Test
    fun `Parsing lambdas works`() {
        // Simple generic type propagation works
        assertEquals(
                6, compile("let sum = 0; NoodleExample.intStream().forEach(|x| sum = sum + x); return sum;").call(
                SimpleEnvironment()
        )
        )
        // Class derived generic type propagation works
        assertEquals(
                0,
                compile("let sum = 0; NoodleExample.stream(java.lang.Integer.class).forEach(|x| sum = sum + x); return sum;").call(
                        SimpleEnvironment()
                )
        )
        // Object derived generic type propagation works
        assertEquals(
                42,
                compile("let sum = 0; NoodleExample.singletonStream(42).forEach(|x| sum = sum + x); return sum;").call(
                        SimpleEnvironment()
                )
        )
        // Var-args generic type propagation works
        assertEquals(
                7, compile("let sum = 0; java.util.Arrays.asList(3,4).forEach(|x| sum = sum + x); return sum;").call(
                SimpleEnvironment()
        )
        )
        // Zero-arg lambdas work
        assertEquals(
                42, compile("let x = 0; NoodleExample.invokeUnitOfWork(|| x = 42); return x;").call(SimpleEnvironment())
        )
    }

    @Test
    fun `type propagation with generic lambdas work`() {
        // We expect the following to compile properly by forwarding the type from the first parameter to the second...
        compile("NoodleExample.propagateTypes(String.class, |s| { s.startsWith('x'); })")
    }

    @Test
    fun `Exceptions in lambdas work`() {
        assertThrows<ScriptingException> {
            // An undeclared exception is thrown within a lambda...
            // The exception is turned into a ScriptingException if it is not throwable (undeclared) within a lambda...
            compile("NoodleExample.invokeConsumer(|x| { NoodleExample.throwDeclaredException(); })").call(
                    SimpleEnvironment()
            )
        }
        assertThrows<ScriptingException> {
            // A RuntimeException is thrown...
            // The exception is re-thrown as it doesn't need to be declared...
            compile("NoodleExample.invokeConsumer(|x| { x / 0; })").call(SimpleEnvironment())
        }
    }

    @ParameterizedTest
    @CsvSource(
            delimiter = '|', useHeadersInDisplayName = true, textBlock = """
         input                                      | output
        'false'                                     | false
        'true'                                      | true
        'false && true'                             | false
        'true && true'                              | true
        'false || false'                            | false
        'false || true'                             | true
        'null.as(java.lang.Boolean.class) && true'  | false
        'null.as(java.lang.Boolean.class) && false' | false
        'null.as(java.lang.Boolean.class) || true'  | true
        'null.as(java.lang.Boolean.class) || false' | false"""
    )
    fun `Conditions work as expected`(input: String, output: Boolean) {
        assertEquals(output, compile(input).call(SimpleEnvironment()))
    }

    @Test
    fun `Interfaces support Object methods`() {
        assert(
                compile("NoodleExample.intConsumer().getClass().getName()").call(SimpleEnvironment()).toString()
                        .startsWith("sirius.pasta.noodle.compiler.NoodleExample")
        )
    }

    @Test
    fun `Types can be derived from generic super classes`() {
        assertEquals(
                "42",
                compile("NoodleExample.longToString(NoodleExample.INSTANCE.getRef().getId())").call(SimpleEnvironment())
        )
        assertEquals(
                "Hello World",
                compile("Strings.join(' ', NoodleExample.INSTANCE.getRef().getTest(), 'World')").call(SimpleEnvironment())
        )
    }

    @Test
    fun `Incomplete class literals are detected and reported`() {
        val compilationContext =
                CompilationContext(SourceCodeInfo.forInlineCode("part(sirius.pasta.tagliatelle.Tagliatelle).getExtensions(null)"))
        NoodleCompiler(compilationContext).compileScript()

        assertEquals(1, compilationContext.errors.size)
        assertEquals(
                "  1: 6: Found an incomplete class literal 'class sirius.pasta.tagliatelle.Tagliatelle'. Add '.class' if you want to refer to the class object.",
                compilationContext.errors[0].message
        )
    }

    @Test
    fun `Calling constructors works`() {
        // calling a constructor works as expected...
        assertEquals("A", compile("Tuple.new('A','B').getFirst()").call(SimpleEnvironment()))
        // generic parameter propagation works as expected...
        assertEquals(
                "java.lang.String",
                compile("Tuple.new('A', 1).getFirst().getClass().getName()").call(SimpleEnvironment())
        )
        assertEquals(
                "java.lang.Integer",
                compile("Tuple.new('A', 1).getSecond().getClass().getName()").call(SimpleEnvironment())
        )
    }

    @Test
    fun `Calling varags works`() {
        // Invoking a vararg with a pre-baked array works as expected (the array is used as varargs)
        assertEquals(3, compile("java.util.Arrays.asList(NoodleExample.AN_ARRAY).size()").call(SimpleEnvironment()))
        // Collecting additional parameters into an array still works...
        assertEquals(3, compile("java.util.Arrays.asList('a', 'b', 'c').size()").call(SimpleEnvironment()))
        // ..even with only a single parameter...
        assertEquals(1, compile("java.util.Arrays.asList('a').size()").call(SimpleEnvironment()))
    }
}
