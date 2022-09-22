/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle

import sirius.kernel.tokenizer.Position
import sirius.kernel.BaseSpecification
import sirius.pasta.noodle.compiler.CompilationContext

class CompilationContextSpec extends BaseSpecification {

    def "resolving a classes works"() {
        when:
        CompilationContext compilationContext = new CompilationContext()
        compilationContext.addImport(Position.UNKNOWN,  "System", System.class);
        then: "Resolving fully qualified classes works"
        compilationContext.tryResolveClass("java.lang.String").get() == String.class
        and: "Aliases work"
        compilationContext.tryResolveClass("String").get() == String.class
        and: "Imports work"
        compilationContext.tryResolveClass("System").get() == System.class
    }

}
