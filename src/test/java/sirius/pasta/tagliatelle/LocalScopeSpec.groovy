/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.tagliatelle

import parsii.tokenizer.ParseError
import sirius.kernel.BaseSpecification
import sirius.kernel.di.std.Part
import sirius.pasta.noodle.compiler.CompileError
import sirius.pasta.noodle.compiler.CompileException
import sirius.pasta.tagliatelle.compiler.TemplateCompiler
import sirius.web.resources.Resources

class LocalScopeSpec extends BaseSpecification {

    @Part
    private static Tagliatelle tagliatelle

    @Part
    private static Resources resources

    @Part
    private static TestHelper test

    def "locals within loops cleanup works"() {
        given:
        List<String> list = ["a", "b", "c"]
        String expectedResult = resources.resolve("templates/local-scope.html").get().getContentAsString()
        when:
        def ctx = tagliatelle.
                createResourceCompilationContext("test",
                                                 resources.resolve("/templates/local-scope.html.pasta").get(),
                                                 null)
        List<CompileError> errors = new TemplateCompiler(ctx).compile()
        then:
        errors.size() == 0
        and:
        test.basicallyEqual(ctx.getTemplate().renderToString(list), expectedResult)
    }

    def "failing access out of scope for loops and blocks works"() {
        given:
        List<String> list = ["a", "b", "c"]
        List<CompileError> errors
        when:
        try {
            def ctx = tagliatelle.
                    createResourceCompilationContext("test",
                                                     resources.resolve("/templates/out-of-scope.html.pasta").get(),
                                                     null)
            new TemplateCompiler(ctx).compile()
        } catch (CompileException err) {
            errors = err.getErrors()
        }
        then:
        errors.size() == 5
        errors.get(0).getError().getSeverity() == ParseError.Severity.ERROR
        errors.get(0).toString().contains("Unknown variable test")
        errors.get(1).getError().getSeverity() == ParseError.Severity.ERROR
        errors.get(1).toString().contains("Unknown variable el")
        errors.get(2).getError().getSeverity() == ParseError.Severity.ERROR
        errors.get(2).toString().contains("Unknown variable test")
        errors.get(3).getError().getSeverity() == ParseError.Severity.ERROR
        errors.get(3).toString().contains("Unknown variable el")
        errors.get(4).getError().getSeverity() == ParseError.Severity.ERROR
        errors.get(4).toString().contains("Unknown variable test")
    }

    def "failing access out of scope for if works"() {
        when:
        List<CompileError> errors
        try {
            def source = "<i:if test=\"1 == 1\"><i:local name=\"test\" value=\"1\"/></i:if>@test " +
                    "@if(1 == 1){<i:local name=\"test\" value=\"1\"/>} @test"
            def ctx = tagliatelle.createInlineCompilationContext("test", source, null)
            new TemplateCompiler(ctx,).compile()
        } catch (CompileException err) {
            errors = err.getErrors()
        }
        then:
        errors.size() == 2
        errors.get(0).getError().getSeverity() == ParseError.Severity.ERROR
        errors.get(0).toString().contains("Unknown variable test")
        errors.get(1).getError().getSeverity() == ParseError.Severity.ERROR
        errors.get(1).toString().contains("Unknown variable test")
    }

    def "failing access out of scope for else works"() {
        when:
        List<CompileError> errors
        try {
            def source = "<i:if test=\"1 == 1\"><i:else><i:local name=\"test\" value=\"1\"/></i:else></i:if>@test"
            def ctx = tagliatelle.createInlineCompilationContext("test", source, null)
            new TemplateCompiler(ctx).compile()
        } catch (CompileException err) {
            errors = err.getErrors()
        }
        then:
        errors.size() == 1
        errors.get(0).getError().getSeverity() == ParseError.Severity.ERROR
        errors.get(0).toString().contains("Unknown variable test")
    }

    def "failing access out of scope for render works"() {
        when:
        List<CompileError> errors
        try {
            def source = "<e:scope>@outer</e:scope>"
            def ctx = tagliatelle.createInlineCompilationContext("test", source, null)
            new TemplateCompiler(ctx,).compile()
        } catch (CompileException err) {
            errors = err.getErrors()
        }
        then:
        errors.size() == 1
        errors.get(0).getError().getSeverity() == ParseError.Severity.ERROR
        errors.get(0).toString().contains("Unknown variable outer")
    }

    def "failing access out of scope for invoke works"() {
        when:
        List<CompileError> errors
        try {
            def source = "<i:invoke template=\"/templates/invoke-scope.html.pasta\"/>@invokeLocal"
            def ctx = tagliatelle.createInlineCompilationContext("test", source, null)
            new TemplateCompiler(ctx).compile()
        } catch (CompileException err) {
            errors = err.getErrors()
        }
        then:
        errors.size() == 1
        errors.get(0).getError().getSeverity() == ParseError.Severity.ERROR
        errors.get(0).toString().contains("Unknown variable invokeLocal")
    }

    def "failing access out of scope for include works"() {
        when:
        List<CompileError> errors
        try {
            def source = "<i:include name=\"/templates/invoke-scope.html.pasta\"/>@invokeLocal"
            def ctx = tagliatelle.createInlineCompilationContext("test", source, null)
            new TemplateCompiler(ctx).compile()
        } catch (CompileException err) {
            errors = err.getErrors()
        }
        then:
        errors.size() == 2
        errors.get(1).getError().getSeverity() == ParseError.Severity.ERROR
        errors.get(1).toString().contains("Unknown variable invokeLocal")
    }

    def "failing access out of scope for if/else works"() {
        when:
        List<CompileError> errors
        try {
            def source = "<i:if test=\"true\"><i:local name=\"test\" value=\"1\"/><i:else>@test</i:else></i:if>" +
                    "@if (true){<i:local name=\"test\" value=\"1\"/>}else{@test}"
            def ctx = tagliatelle.createInlineCompilationContext("test", source, null)
            new TemplateCompiler(ctx).compile()
        } catch (CompileException err) {
            errors = err.getErrors()
        }
        then:
        errors.size() == 2
        errors.get(0).getError().getSeverity() == ParseError.Severity.ERROR
        errors.get(0).toString().contains("Unknown variable test")
        errors.get(1).getError().getSeverity() == ParseError.Severity.ERROR
        errors.get(1).toString().contains("Unknown variable test")
    }
}
