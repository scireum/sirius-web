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
import sirius.kernel.commons.Amount
import sirius.kernel.commons.Tuple
import sirius.kernel.commons.Value
import sirius.kernel.di.std.Part
import sirius.tagliatelle.compiler.CompilationContext
import sirius.tagliatelle.compiler.CompileError
import sirius.tagliatelle.compiler.CompileException
import sirius.tagliatelle.compiler.Compiler
import sirius.tagliatelle.rendering.GlobalRenderContext
import sirius.web.resources.Resource
import sirius.web.resources.Resources

import java.time.LocalDate

class CompilerSpec extends BaseSpecification {

    @Part
    private static Tagliatelle tagliatelle

    @Part
    private static Resources resources

    @Part
    private static TestHelper test

    def "vararg detection doesn't crash when a non-vararg method is invoked with null"() {
        when:
        def ctx = new CompilationContext(new Template("test", null), null)
        List<CompileError> errors = new Compiler(ctx,
                                                 "<i:arg type=\"sirius.kernel.commons.Value\" name=\"test\" /> @test.asLocalDate(null)").
                compile()
        then:
        errors.size() == 0
        and:
        ctx.getTemplate().renderToString(Value.of(LocalDate.now())) != ""
    }

    def "vararg detection works without parameters"() {
        when:
        def ctx = new CompilationContext(new Template("test", null), null)
        List<CompileError> errors = new Compiler(ctx,
                                                 "<i:arg type=\"sirius.kernel.commons.Value\" name=\"test\" />@test.ignore().asString()").
                compile()
        then:
        errors.size() == 0
        and:
        ctx.getTemplate().renderToString(Value.of("test")) == "test"
    }

    def "vararg detection works with a single parameter"() {
        when:
        def ctx = new CompilationContext(new Template("test", null), null)
        List<CompileError> errors = new Compiler(ctx,
                                                 "<i:arg type=\"sirius.kernel.commons.Value\" name=\"test\" />@test.ignore('test').asString()").
                compile()
        then:
        errors.size() == 0
        and:
        ctx.getTemplate().renderToString(Value.of("test")) == ""
    }

    def "vararg detection works with null as parameter"() {
        when:
        def ctx = new CompilationContext(new Template("test", null), null)
        List<CompileError> errors = new Compiler(ctx,
                                                 "<i:arg type=\"sirius.kernel.commons.Value\" name=\"test\" />@test.ignore(null).asString()").
                compile()
        then:
        errors.size() == 0
        and:
        ctx.getTemplate().renderToString(Value.of("test")) == "test"
    }

    def "method overloading works with generics"() {
        when:
        def ctx = new CompilationContext(new Template("test", null), null)
        List<CompileError> errors = new Compiler(ctx, "<i:arg type=\"sirius.tagliatelle.TestObject\" name=\"test\" />" +
                "<i:arg type=\"sirius.kernel.commons.Amount\" name=\"test1\" />" +
                "@test.genericTest(test1)").compile()
        then:
        errors.size() == 0
        and:
        ctx.getTemplate().renderToString(TestObject.INSTANCE, Amount.TEN) == "-10"
    }

    def "generic type propagation works"() {
        when:
        def ctx = new CompilationContext(new Template("test", null), null)
        List<CompileError> errors = new Compiler(ctx, "<i:arg type=\"sirius.tagliatelle.TestObject\" name=\"test\" />" +
                "@test.getGenericReturnType().getFirst().apply('Test').get().as(String.class).substring(1)").compile()
        then:
        errors.size() == 1
        and: "We expect a warning as our cast to String is now unnecessary due to proper generic propagation"
        errors.get(0).getError().getSeverity() == ParseError.Severity.WARNING
        and:
        ctx.getTemplate().renderToString(TestObject.INSTANCE) == "est"
    }

    def "generic type propagation works for class parameters"() {
        when:
        def ctx = new CompilationContext(new Template("test", null), null)
        List<CompileError> errors = new Compiler(ctx,
                                                 "@user.getHelper(sirius.tagliatelle.ExampleHelper.class).getTestValue()").
                compile()
        then:
        errors.size() == 0
        and:
        ctx.getTemplate().renderToString(TestObject.INSTANCE) == "test"
    }

    def "vararg detection works with several parameters"() {
        when:
        def ctx = new CompilationContext(new Template("test", null), null)
        List<CompileError> errors = new Compiler(ctx,
                                                 "<i:arg type=\"sirius.kernel.commons.Value\" name=\"test\" />@test.ignore('a', 'test').asString()").
                compile()
        then:
        errors.size() == 0
        and:
        ctx.getTemplate().renderToString(Value.of("test")) == ""
    }

    def "vararg detection works without vararg parameters"() {
        when:
        def ctx = new CompilationContext(new Template("test", null), null)
        List<CompileError> errors = new Compiler(ctx,
                                                 "<i:arg type=\"sirius.tagliatelle.TestObject\" name=\"test\" />@test.varArgTest('X')").
                compile()
        then:
        errors.size() == 0
        and:
        ctx.getTemplate().renderToString(TestObject.INSTANCE) == "X"
    }

    def "vararg detection works with a single vararg parameter"() {
        when:
        def ctx = new CompilationContext(new Template("test", null), null)
        List<CompileError> errors = new Compiler(ctx,
                                                 "<i:arg type=\"sirius.tagliatelle.TestObject\" name=\"test\" />@test.varArgTest('%s','X')").
                compile()
        then:
        errors.size() == 0
        and:
        ctx.getTemplate().renderToString(TestObject.INSTANCE) == "X"
    }

    def "vararg detection works with null as a vararg parameter"() {
        when:
        def ctx = new CompilationContext(new Template("test", null), null)
        List<CompileError> errors = new Compiler(ctx,
                                                 "<i:arg type=\"sirius.tagliatelle.TestObject\" name=\"test\" />@test.varArgTest('%s', null)").
                compile()
        then:
        errors.size() == 0
        and:
        ctx.getTemplate().renderToString(TestObject.INSTANCE) == "null"
    }

    def "vararg detection works with several vararg parameters"() {
        when:
        def ctx = new CompilationContext(new Template("test", null), null)
        List<CompileError> errors = new Compiler(ctx,
                                                 "<i:arg type=\"sirius.tagliatelle.TestObject\" name=\"test\" />@test.varArgTest('%s %s', 'X', 'Y')").
                compile()
        then:
        errors.size() == 0
        and:
        ctx.getTemplate().renderToString(TestObject.INSTANCE) == "X Y"
    }

    def "nesting of { brackets works as expected"() {
        given:
        String expectedResult = resources.resolve("templates/brackets.html").get().getContentAsString()
        when:
        String result = tagliatelle.resolve("/templates/brackets.html.pasta").get().renderToString()
        then:
        test.basicallyEqual(result, expectedResult)
    }

    def "loops and the loop state work as expected"() {
        given:
        String expectedResult = resources.resolve("templates/loop.html").get().getContentAsString()
        when:
        String result = tagliatelle.resolve("/templates/loop.html.pasta").
                get().
                renderToString(Arrays.asList(1, 2, 3, 4, 5))
        then:
        test.basicallyEqual(result, expectedResult)
    }

    def "switch works as expected"() {
        when:
        String a = tagliatelle.resolve("/templates/switch.html.pasta").get().renderToString("a")
        String b = tagliatelle.resolve("/templates/switch.html.pasta").get().renderToString("b")
        String c = tagliatelle.resolve("/templates/switch.html.pasta").get().renderToString("c")
        then:
        test.basicallyEqual(a, "a")
        test.basicallyEqual(b, "b")
        test.basicallyEqual(c, "c")
    }

    def "extensions work"() {
        when:
        String result = tagliatelle.resolve("/templates/extended.html.pasta").get().renderToString()
        then:
        result == "b"
    }

    def "dynamicInvoke works"() {
        given:
        String expectedResult = resources.resolve("templates/dynamic-invoke.html").get().getContentAsString()
        when:
        String result = tagliatelle.resolve("/templates/dynamic-invoke-outer.html.pasta").get().renderToString()
        then:
        test.basicallyEqual(result, expectedResult)
    }

    def "renderToString works"() {
        given:
        String expectedResult = resources.resolve("templates/render-to-string.html").get().getContentAsString()
        when:
        String result = tagliatelle.resolve("/templates/render-to-string.html.pasta").get().renderToString()
        then:
        test.basicallyEqual(result, expectedResult)
    }

    def "define and xml and json macros works"() {
        given:
        String expectedResult = resources.resolve("templates/define.html").get().getContentAsString()
        when:
        String result = tagliatelle.resolve("/templates/define.html.pasta").get().renderToString()
        then:
        test.basicallyEqual(result, expectedResult)
    }

    def "missing tag detection works"() {
        when:
        List<CompileError> errors = null
        try {
            tagliatelle.resolve("/templates/missing-tag.html.pasta").get()
        } catch (CompileException err) {
            errors = err.getErrors()
        }
        then:
        errors.size() == 2
        errors.get(0).toString().contains("Cannot find a template for the tag: w:unknown")
        errors.get(1).toString().contains("Cannot find a handler for the internal tag: i:unknown")
    }

    private CompilationContext compile(String templateName) {
        Resource resource = resources.resolve(templateName).orElse(null)
        CompilationContext compilationContext = tagliatelle.createCompilationContext(templateName, resource, null)
        Compiler compiler = new Compiler(compilationContext, resource.getContentAsString())
        compiler.compile()
        return compilationContext
    }

    def "argument deprecation is detected"() {
        when:
        CompilationContext compilationContext = compile("/templates/deprecatedArgument.html.pasta")
        List<ParseError> errors = compilationContext.getErrors()
        then:
        errors.size() == 1
        errors.get(0).getSeverity() == ParseError.Severity.WARNING
        errors.get(0).getMessage().contains("The attribute 'deprecatedArg' is deprecated: Do not use")
    }

    def "deprecation is detected"() {
        when:
        CompilationContext compilationContext = compile("/templates/deprecatedCaller.html.pasta")
        List<ParseError> errors = compilationContext.getErrors()
        then:
        errors.size() == 2
        errors.get(0).getSeverity() == ParseError.Severity.WARNING
        errors.get(0).getMessage().contains("The template '<e:deprecated>' is deprecated: Test of deprecated")
    }

    def "calling a deprecated method is detected"() {
        when:
        CompilationContext compilationContext = compile("/templates/deprecatedMethodCall.html.pasta")
        List<ParseError> errors = compilationContext.getErrors()
        then:
        errors.size() == 1
        errors.get(0).getSeverity() == ParseError.Severity.WARNING
        errors.get(0)
              .getMessage()
              .contains("The method sirius.tagliatelle.TestObject.deprecatedMethod is marked as deprecated")
    }

    def "calling a deprecated macro is detected"() {
        when:
        CompilationContext compilationContext = compile("/templates/deprecatedMacroCall.html.pasta")
        List<ParseError> errors = compilationContext.getErrors()
        then:
        errors.size() == 1
        errors.get(0).getSeverity() == ParseError.Severity.WARNING
        errors.get(0)
              .getMessage()
              .contains("The macro deprecatedMacro (sirius.tagliatelle.DeprecatedMacro) is deprecated.")
    }

    def "invalid varargs are detected"() {
        when:
        List<CompileError> errors = null
        try {
            tagliatelle.resolve("/templates/invalid-argument-caller.html.pasta").get()
        } catch (CompileException err) {
            errors = err.getErrors()
        }
        then:
        errors.size() == 2
        errors.get(0).getError().getSeverity() == ParseError.Severity.ERROR
        errors.get(0)
              .toString()
              .contains(
                      "Incompatible attribute types. e:invalidArgumentTaglib expects int for 'invalidArgument', but class java.lang.String was given.")
        !errors.get(1).toString().contains("NullPointerException")
    }

    def "time macros state work as expected"() {
        given:
        String expectedResult = resources.resolve("templates/timeMacros.html").get().getContentAsString()
        when:
        String result = tagliatelle.resolve("/templates/timeMacros.html.pasta").get().renderToString()
        then:
        test.basicallyEqual(result, expectedResult)
    }

    def "different macro call syntax"() {
        given:
        String expectedResult = resources.resolve("templates/macroSyntax.html").get().getContentAsString()
        when:
        String result = tagliatelle.resolve("/templates/macroSyntax.html.pasta").get().renderToString()
        then:
        test.basicallyEqual(result, expectedResult)
    }

    def "jsTemplate taglib works and prevents XSS"() {
        given:
        String expectedResult = resources.resolve("templates/js-template.html").get().getContentAsString()
        when:
        String result = tagliatelle.resolve("/templates/js-template.html.pasta").get().renderToString()
        then:
        test.basicallyEqual(result, expectedResult)
    }


    def "attribute expressions work"() {
        given:
        String expectedResult = resources.resolve("templates/attribute-expressions.html").get().getContentAsString()
        when:
        String result = tagliatelle.resolve("/templates/attribute-expressions.html.pasta").get().renderToString()
        then:
        test.basicallyEqual(result, expectedResult)
    }

    def "validate recursion of a template"() {
        given:
        String expectedResult = resources.resolve("templates/recursion.html").get().getContentAsString()
        when:
        String result = tagliatelle.resolve("/templates/recursion.html.pasta").get().renderToString()
        then:
        test.basicallyEqual(result, expectedResult)
    }

    /**
     * Previously a non-breaking whitespace (U00A0) lead to an endless loop creating an infinita amount of errors.
     */
    def "horror whitespaces don't crash the compiler"() {
        when:
        def ctx = new CompilationContext(new Template("test", null), null)
        List<CompileError> errors = new Compiler(ctx,
                                                 "<i:invoke\u00A0template=\"/templates/attribute-expressions.html.pasta\"/>").
                compile()
        then:
        errors.size() == 1
    }

    def "validate invoke: calling original template from customization"() {
        given:
        String expectedResult = resources.resolve("templates/invoke-customized.html").get().getContentAsString()
        when:
        String result = tagliatelle.resolve("/templates/invoke-customized.html.pasta").get().renderToString()
        String resultCached = tagliatelle.resolve("/templates/invoke-customized.html.pasta").get().renderToString()
        then:
        test.basicallyEqual(result, expectedResult)
        test.basicallyEqual(resultCached, expectedResult)
    }

    def "casting to inner class works"() {
        given:
        InnerClassTestObject.InnerClass innerClass = new InnerClassTestObject.InnerClass()
        innerClass.setTest("test")
        String expectedResult = "test"
        when:
        String result = tagliatelle.resolve("/templates/inner-class.html.pasta")
                                   .get().renderToString(Tuple.create(innerClass, innerClass))
        then:
        test.basicallyEqual(result, expectedResult)
    }

    def "Toplevel i:block and i:extraBlock (even when nested) produce the appropriate extra outputs"() {
        when:
        def compilationContext = new CompilationContext(new Template("test", null), null)
        List<CompileError> errors = new Compiler(compilationContext, "<i:block name=\"test\">" +
                "Test" +
                "<i:extraBlock name=\"extra-test\">Extra Test</i:extraBlock>" +
                "</i:block>").
                compile()
        and:
        GlobalRenderContext globalRenderContext = tagliatelle.createRenderContext()
        and:
        compilationContext.getTemplate().render(globalRenderContext)
        then:
        errors.size() == 0
        and: "A top level i:block is expected to render its contents into an extra block"
        globalRenderContext.getExtraBlock("test") == "Test"
        and: "An i:extraBlock is expected to do the same - independently of its nesting and location"
        globalRenderContext.getExtraBlock("extra-test") == "Extra Test"
    }
}
