/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.compiler


import org.junit.jupiter.api.Test
import org.junit.jupiter.api.extension.ExtendWith
import sirius.kernel.Setup
import sirius.kernel.SiriusExtension
import sirius.pasta.noodle.compiler.ir.MethodCall
import sirius.pasta.noodle.compiler.ir.Node
import kotlin.test.assertEquals
import kotlin.test.assertTrue

/**
 * Tests the Noodle [Parser].
 */
@ExtendWith(SiriusExtension::class)
class ParserTest {

    private fun parse(input: String): Node {
        val compilationContext = CompilationContext(SourceCodeInfo.forInlineCode(input))
        return Parser(compilationContext, compilationContext.sourceCodeInfo.createReader()).parseExpression(
                true
        ).reduce(compilationContext)
    }

    @Test
    fun `Comments work`() {
        assertEquals(4, parse("1 + 3 // - 5").constantValue)
        assertEquals(4, parse("7 /*+ 3 */ - 3").constantValue)
    }

    @Test
    fun `Constant folding works`() {
        assertEquals(4, parse("1 + 3").constantValue)
        assertEquals(13, parse("1 + 3 * 4").constantValue)
        assertEquals(0, parse("(1 + 3) % 2").constantValue)
        assertEquals("13", parse("'1' + '3'").constantValue)
        assertEquals(3, parse("7 - 4").constantValue)
    }

    @Test
    fun `Parsing a class literal works`() {
        assertEquals(System::class.java, parse("java.lang.System.class").constantValue)
        assertEquals(Map.Entry::class.java, parse("Map.Entry.class").constantValue)
        assertEquals(Setup.Mode.DEVELOP, parse("sirius.kernel.Setup.Mode.DEVELOP").constantValue)
        assertEquals(System.out, parse("java.lang.System.out").constantValue)
        assertTrue { parse("java.lang.System.out.println('Hello World')") is MethodCall }
    }

}
