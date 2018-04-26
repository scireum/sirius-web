/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle

import parsii.tokenizer.ParseError
import sirius.kernel.BaseSpecification
import sirius.kernel.commons.Strings
import sirius.kernel.commons.Value
import sirius.kernel.di.std.Part
import sirius.tagliatelle.compiler.CompilationContext
import sirius.tagliatelle.compiler.CompileError
import sirius.tagliatelle.compiler.CompileException
import sirius.tagliatelle.compiler.Compiler
import sirius.web.resources.Resources

import java.time.LocalDate

class CompilerSpec extends BaseSpecification {

    @Part
    private static Tagliatelle tagliatelle
    @Part
    private static Resources resources

    private boolean basicallyEqual(String left, String right) {
        return Strings.areEqual(left.replaceAll("\\s", ""), right.replaceAll("\\s", ""))
    }

    def "vararg detection doesn't crash when a non-vararg method is invoked with null"() {
        when:
        def ctx = new CompilationContext(new Template("test", null), null)
        List<CompileError> errors = new Compiler(ctx, "<i:arg type=\"sirius.kernel.commons.Value\" name=\"test\" /> @test.asLocalDate(null)").compile()
        then:
        errors.size() == 0
        and:
        ctx.getTemplate().renderToString(Value.of(LocalDate.now())) != ""
    }

    def "vararg detection works without parameters"() {
        when:
        def ctx = new CompilationContext(new Template("test", null), null)
        List<CompileError> errors = new Compiler(ctx, "<i:arg type=\"sirius.kernel.commons.Value\" name=\"test\" />@test.ignore().asString()").compile()
        then:
        errors.size() == 0
        and:
        ctx.getTemplate().renderToString(Value.of("test")) == "test"
    }

    def "vararg detection works with a single parameter"() {
        when:
        def ctx = new CompilationContext(new Template("test", null), null)
        List<CompileError> errors = new Compiler(ctx, "<i:arg type=\"sirius.kernel.commons.Value\" name=\"test\" />@test.ignore('test').asString()").compile()
        then:
        errors.size() == 0
        and:
        ctx.getTemplate().renderToString(Value.of("test")) == ""
    }

    def "vararg detection works with null as parameter"() {
        when:
        def ctx = new CompilationContext(new Template("test", null), null)
        List<CompileError> errors = new Compiler(ctx, "<i:arg type=\"sirius.kernel.commons.Value\" name=\"test\" />@test.ignore(null).asString()").compile()
        then:
        errors.size() == 0
        and:
        ctx.getTemplate().renderToString(Value.of("test")) == "test"
    }

    def "vararg detection works with several parameters"() {
        when:
        def ctx = new CompilationContext(new Template("test", null), null)
        List<CompileError> errors = new Compiler(ctx, "<i:arg type=\"sirius.kernel.commons.Value\" name=\"test\" />@test.ignore('a', 'test').asString()").compile()
        then:
        errors.size() == 0
        and:
        ctx.getTemplate().renderToString(Value.of("test")) == ""
    }

    def "vararg detection works without vararg parameters"() {
        when:
        def ctx = new CompilationContext(new Template("test", null), null)
        List<CompileError> errors = new Compiler(ctx, "<i:arg type=\"sirius.tagliatelle.TestObject\" name=\"test\" />@test.varArgTest('X')").compile()
        then:
        errors.size() == 0
        and:
        ctx.getTemplate().renderToString(TestObject.INSTANCE) == "X"
    }

    def "vararg detection works with a single vararg parameter"() {
        when:
        def ctx = new CompilationContext(new Template("test", null), null)
        List<CompileError> errors = new Compiler(ctx, "<i:arg type=\"sirius.tagliatelle.TestObject\" name=\"test\" />@test.varArgTest('%s','X')").compile()
        then:
        errors.size() == 0
        and:
        ctx.getTemplate().renderToString(TestObject.INSTANCE) == "X"
    }

    def "vararg detection works with null as a vararg parameter"() {
        when:
        def ctx = new CompilationContext(new Template("test", null), null)
        List<CompileError> errors = new Compiler(ctx, "<i:arg type=\"sirius.tagliatelle.TestObject\" name=\"test\" />@test.varArgTest('%s', null)").compile()
        then:
        errors.size() == 0
        and:
        ctx.getTemplate().renderToString(TestObject.INSTANCE) == "null"
    }

    def "vararg detection works with several vararg parameters"() {
        when:
        def ctx = new CompilationContext(new Template("test", null), null)
        List<CompileError> errors = new Compiler(ctx, "<i:arg type=\"sirius.tagliatelle.TestObject\" name=\"test\" />@test.varArgTest('%s %s', 'X', 'Y')").compile()
        then:
        errors.size() == 0
        and:
        ctx.getTemplate().renderToString(TestObject.INSTANCE) == "X Y"
    }

    def "nesting of { brackets works as expected"() {
        given:
        String expectedResult = resources.resolve("templates/brackets.html").get().getContentAsString()
        when:
        String result = tagliatelle.resolve("templates/brackets.html.pasta").get().renderToString()
        then:
        basicallyEqual(result, expectedResult)
    }

    def "loops and the loop state work as expected"() {
        given:
        String expectedResult = resources.resolve("templates/loop.html").get().getContentAsString()
        when:
        String result = tagliatelle.resolve("templates/loop.html.pasta").get().renderToString(Arrays.asList(1, 2, 3, 4, 5))
        then:
        basicallyEqual(result, expectedResult)
    }

    def "dynamicInvoke works"() {
        given:
        String expectedResult = resources.resolve("templates/dynamic-invoke.html").get().getContentAsString()
        when:
        String result = tagliatelle.resolve("templates/dynamic-invoke-outer.html.pasta").get().renderToString()
        then:
        basicallyEqual(result, expectedResult)
    }

    def "renderToString works"() {
        given:
        String expectedResult = resources.resolve("templates/render-to-string.html").get().getContentAsString()
        when:
        String result = tagliatelle.resolve("templates/render-to-string.html.pasta").get().renderToString()
        then:
        basicallyEqual(result, expectedResult)
    }

    def "define and xml and json macros works"() {
        given:
        String expectedResult = resources.resolve("templates/define.html").get().getContentAsString()
        when:
        String result = tagliatelle.resolve("templates/define.html.pasta").get().renderToString()
        then:
        basicallyEqual(result, expectedResult)
    }

    def "missing tag detection works"() {
        when:
        List<CompileError> errors = null
        try {
            tagliatelle.resolve("templates/missing-tag.html.pasta").get()
        } catch (CompileException err) {
            errors = err.getErrors()
        }
        then:
        errors.size() == 2
        errors.get(0).toString().contains("Cannot find a template for the tag: w:unknown")
        errors.get(1).toString().contains("Cannot find a handler for the internal tag: i:unknown")
    }

    def "argument deprecation is detected"() {
        when:
        List<CompileError> errors = null
        try {
            tagliatelle.resolve("templates/deprecatedArgument.html.pasta").get()
        } catch (CompileException err) {
            errors = err.getErrors()
        }
        then:
        errors.size() == 1
        errors.get(0).getError().getSeverity() == ParseError.Severity.ERROR
        errors.get(0).toString().contains("The attribute 'deprecatedArg' is deprecated: Do not use")
    }

    def "deprecation is detected"() {
        when:
        List<CompileError> errors = null
        try {
            tagliatelle.resolve("templates/deprecatedCaller.html.pasta").get()
        } catch (CompileException err) {
            errors = err.getErrors()
        }
        then:
        errors.size() == 2
        errors.get(0).getError().getSeverity() == ParseError.Severity.ERROR
        errors.get(0).toString().contains("The template '<e:deprecated>' is deprecated: Test of deprecated")
    }

    def "time macros state work as expected"() {
        given:
        String expectedResult = resources.resolve("templates/timeMacros.html").get().getContentAsString()
        when:
        String result = tagliatelle.resolve("templates/timeMacros.html.pasta").get().renderToString()
        then:
        basicallyEqual(result, expectedResult)
    }

    def "different macro call syntax"() {
        given:
        String expectedResult = resources.resolve("templates/macroSyntax.html").get().getContentAsString()
        when:
        String result = tagliatelle.resolve("templates/macroSyntax.html.pasta").get().renderToString()
        then:
        basicallyEqual(result, expectedResult)
    }

    def "jsTemplate taglib works"() {
        given:
        String expectedResult = resources.resolve("templates/js-template.html").get().getContentAsString()
        when:
        String result = tagliatelle.resolve("templates/js-template.html.pasta").get().renderToString()
        then:
        basicallyEqual(result, expectedResult)
    }


    def "attribute expressions work"() {
        given:
        String expectedResult = resources.resolve("templates/attribute-expressions.html").get().getContentAsString()
        when:
        String result = tagliatelle.resolve("templates/attribute-expressions.html.pasta").get().renderToString()
        then:
        basicallyEqual(result, expectedResult)
    }

    def "validate recursion of a template"() {
        given:
        String expectedResult = resources.resolve("templates/recursion.html").get().getContentAsString()
        when:
        String result = tagliatelle.resolve("templates/recursion.html.pasta").get().renderToString()
        then:
        basicallyEqual(result, expectedResult)
    }

}
