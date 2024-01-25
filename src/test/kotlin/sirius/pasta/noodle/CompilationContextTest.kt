/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.extension.ExtendWith
import sirius.kernel.SiriusExtension
import sirius.kernel.tokenizer.Position
import sirius.pasta.noodle.compiler.CompilationContext
import kotlin.test.assertEquals

@ExtendWith(SiriusExtension::class)
class CompilationContextTest {

    @Test
    fun `resolving a classes works`() {
        val compilationContext = CompilationContext(null)
        compilationContext.addImport(Position.UNKNOWN, "System", System::class.java)
        // Resolving fully qualified classes works
        assertEquals(String::class.java, compilationContext.tryResolveClass("java.lang.String").get())
        // Aliases work
        assertEquals(String::class.java, compilationContext.tryResolveClass("String").get())
        // Imports work
        assertEquals(System::class.java, compilationContext.tryResolveClass("System").get())
    }
}
