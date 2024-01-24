/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.compiler


import sirius.kernel.BaseSpecification
import sirius.kernel.Setup
import sirius.pasta.noodle.compiler.ir.MethodCall
import sirius.pasta.noodle.compiler.ir.Node

class ParserTest extends BaseSpecification {

    Node parse(String input) {
        def compilationContext = new CompilationContext(SourceCodeInfo.forInlineCode(input))
        return new Parser(compilationContext, compilationContext.getSourceCodeInfo().createReader()).parseExpression(
        true).reduce(compilationContext)
    }

    def "comments work"() {
        expect:
        parse("1 + 3 // - 5").getConstantValue() == 4
        and:
        parse("7 /*+ 3 */ - 3").getConstantValue() == 4
    }

    def "constant folding works"() {
        expect:
        parse("1 + 3").getConstantValue() == 4
        and:
        parse("1 + 3 * 4").getConstantValue() == 13
        and:
        parse("(1 + 3) % 2").getConstantValue() == 0
        and:
        parse("'1' + '3'").getConstantValue() == "13"
        and:
        parse("7 - 4").getConstantValue() == 3
    }

    def "parsing a class literal works"() {
        expect:
        parse("java.lang.System.class").getConstantValue() == System.class
        and:
                parse("Map.Entry.class").getConstantValue() == Map.Entry.class
        and:
                parse("sirius.kernel.Setup.Mode.DEVELOP").getConstantValue() == Setup.Mode.DEVELOP
        and:
        parse("java.lang.System.out").getConstantValue() == System.out
        and:
        parse("java.lang.System.out.println('Hello World')") instanceof MethodCall
    }

}
