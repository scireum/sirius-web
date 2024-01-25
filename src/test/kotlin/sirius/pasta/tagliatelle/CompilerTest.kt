/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.tagliatelle


import org.junit.jupiter.api.Test
import org.junit.jupiter.api.extension.ExtendWith
import sirius.kernel.SiriusExtension
import sirius.kernel.commons.Amount
import sirius.kernel.commons.Tuple
import sirius.kernel.commons.Value
import sirius.kernel.di.std.Part
import sirius.kernel.tokenizer.ParseError
import sirius.pasta.noodle.compiler.CompileError
import sirius.pasta.noodle.compiler.CompileException
import sirius.pasta.noodle.compiler.SourceCodeInfo
import sirius.pasta.tagliatelle.compiler.TemplateCompilationContext
import sirius.pasta.tagliatelle.compiler.TemplateCompiler
import sirius.web.resources.Resources
import java.time.LocalDate
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertNotEquals
import kotlin.test.assertTrue

/**
 * Tests the [TemplateCompiler] and [Template] classes.
 */
@ExtendWith(SiriusExtension::class)
class CompilerTest {

    @Test
    fun `Vararg detection doesn't crash when a non-vararg method is invoked with null`() {
        val source = "<i:arg type=\"sirius.kernel.commons.Value\" name=\"test\" /> @test.asLocalDate(null)"
        val context = TemplateCompilationContext(Template("test", null), SourceCodeInfo.forInlineCode(source), null)
        val errors = TemplateCompiler(context).compile()

        assertTrue { errors.isEmpty() }
        assertNotEquals("", context.template.renderToString(Value.of(LocalDate.now())))
    }

    @Test
    fun `Vararg detection works without parameters`() {
        val source = "<i:arg type=\"sirius.kernel.commons.Value\" name=\"test\" />@test.ignore().asString()"
        val context = TemplateCompilationContext(Template("test", null), SourceCodeInfo.forInlineCode(source), null)
        val errors = TemplateCompiler(context).compile()

        assertTrue { errors.isEmpty() }
        assertEquals("test", context.template.renderToString(Value.of("test")))
    }

    @Test
    fun `Vararg detection works with a single parameter`() {
        val source = "<i:arg type=\"sirius.kernel.commons.Value\" name=\"test\" />@test.ignore('test').asString()"
        val context = TemplateCompilationContext(Template("test", null), SourceCodeInfo.forInlineCode(source), null)
        val errors = TemplateCompiler(context).compile()

        assertTrue { errors.isEmpty() }
        assertEquals("", context.template.renderToString(Value.of("test")))
    }

    @Test
    fun `Vararg detection works with null as parameter`() {
        val source = "<i:arg type=\"sirius.kernel.commons.Value\" name=\"test\" />@test.ignore(null).asString()"
        val context = TemplateCompilationContext(Template("test", null), SourceCodeInfo.forInlineCode(source), null)
        val errors = TemplateCompiler(context).compile()

        assertTrue { errors.isEmpty() }
        assertEquals("test", context.template.renderToString(Value.of("test")))
    }

    @Test
    fun `Method overloading works with generics`() {
        val source =
                "<i:arg type=\"sirius.pasta.tagliatelle.TestObject\" name=\"test\" />" + "<i:arg type=\"sirius.kernel.commons.Amount\" name=\"test1\" />" + "@test.genericTest(test1)"
        val context = TemplateCompilationContext(Template("test", null), SourceCodeInfo.forInlineCode(source), null)
        val errors = TemplateCompiler(context).compile()

        assertTrue { errors.isEmpty() }
        assertEquals("-10", context.template.renderToString(TestObject.INSTANCE, Amount.TEN))
    }

    @Test
    fun `Generic type propagation works`() {
        val source =
                "<i:arg type=\"sirius.pasta.tagliatelle.TestObject\" name=\"test\" />" + "@test.getGenericReturnType().getFirst().apply('Test').get().as(String.class).substring(1)"
        val context = TemplateCompilationContext(Template("test", null), SourceCodeInfo.forInlineCode(source), null)
        val errors = TemplateCompiler(context).compile()

        // We expect a warning as our cast to String is now unnecessary due to proper generic propagation
        assertEquals(1, errors.size)
        assertEquals(ParseError.Severity.WARNING, errors[0].error.severity)
        assertEquals("est", context.template.renderToString(TestObject.INSTANCE))
    }

    @Test
    fun `Generic type propagation works with null`() {
        val source =
                "<i:arg type=\"sirius.pasta.tagliatelle.TestObject\" name=\"test\"/>" + "@test.emptyOptional().orElse(null).substring(1)"
        val context = TemplateCompilationContext(Template("test", null), SourceCodeInfo.forInlineCode(source), null)
        val errors = TemplateCompiler(context).compile()

        assertTrue { errors.isEmpty() }

        assertEquals("", context.template.renderToString(TestObject.INSTANCE))
    }

    @Test
    fun `Generic type propagation works for class parameters`() {
        val source = "@helper(sirius.pasta.tagliatelle.ExampleHelper.class).getTestValue()"
        val context = TemplateCompilationContext(Template("test", null), SourceCodeInfo.forInlineCode(source), null)
        val errors = TemplateCompiler(context).compile()

        assertTrue { errors.isEmpty() }
        assertEquals("test", context.template.renderToString(TestObject.INSTANCE))
    }

    @Test
    fun `Vararg detection works with several parameters`() {
        val source = "<i:arg type=\"sirius.kernel.commons.Value\" name=\"test\" />@test.ignore('a', 'test').asString()"
        val context = TemplateCompilationContext(Template("test", null), SourceCodeInfo.forInlineCode(source), null)
        val errors = TemplateCompiler(context).compile()

        assertTrue { errors.isEmpty() }

        assertEquals("", context.template.renderToString(Value.of("test")))
    }

    @Test
    fun `Vararg detection works without vararg parameters`() {
        val source = "<i:arg type=\"sirius.pasta.tagliatelle.TestObject\" name=\"test\" />@test.varArgTest('X')"
        val context = TemplateCompilationContext(Template("test", null), SourceCodeInfo.forInlineCode(source), null)
        val errors = TemplateCompiler(context).compile()

        assertTrue { errors.isEmpty() }
        assertEquals("X", context.template.renderToString(TestObject.INSTANCE))
    }

    @Test
    fun `Vararg detection works with a single vararg parameter`() {
        val source = "<i:arg type=\"sirius.pasta.tagliatelle.TestObject\" name=\"test\" />@test.varArgTest('%s','X')"
        val context = TemplateCompilationContext(Template("test", null), SourceCodeInfo.forInlineCode(source), null)
        val errors = TemplateCompiler(context).compile()

        assertTrue { errors.isEmpty() }
        assertEquals("X", context.template.renderToString(TestObject.INSTANCE))
    }

    @Test
    fun `Vararg detection works with null as a vararg parameter`() {
        val source = "<i:arg type=\"sirius.pasta.tagliatelle.TestObject\" name=\"test\" />@test.varArgTest('%s', null)"
        val context = TemplateCompilationContext(Template("test", null), SourceCodeInfo.forInlineCode(source), null)
        val errors = TemplateCompiler(context).compile()

        assertTrue { errors.isEmpty() }
        assertEquals("null", context.template.renderToString(TestObject.INSTANCE))
    }

    @Test
    fun `Vararg detection works with several vararg parameters`() {
        val source =
                "<i:arg type=\"sirius.pasta.tagliatelle.TestObject\" name=\"test\" />@test.varArgTest('%s %s', 'X', 'Y')"
        val context = TemplateCompilationContext(Template("test", null), SourceCodeInfo.forInlineCode(source), null)
        val errors = TemplateCompiler(context).compile()

        assertTrue { errors.isEmpty() }
        assertEquals("X Y", context.template.renderToString(TestObject.INSTANCE))
    }

    @Test
    fun `Nesting of { brackets works as expected`() {
        val expectedResult = resources.resolve("templates/brackets.html").get().contentAsString
        val result = tagliatelle.resolve("/templates/brackets.html.pasta").get().renderToString()

        assertTrue { test.basicallyEqual(result, expectedResult) }
    }

    @Test
    fun `Loops and the loop state work as expected`() {
        val expectedResult = resources.resolve("templates/loop.html").get().contentAsString
        val result = tagliatelle.resolve("/templates/loop.html.pasta").get().renderToString(listOf(1, 2, 3, 4, 5))

        assertTrue { test.basicallyEqual(result, expectedResult) }
    }

    @Test
    fun `If works as expected`() {
        val a = tagliatelle.resolve("/templates/if.html.pasta").get().renderToString("a")
        val b = tagliatelle.resolve("/templates/if.html.pasta").get().renderToString("b")

        assertTrue { test.basicallyEqual(a, "a") }
        assertTrue { test.basicallyEqual(b, "b") }
    }

    @Test
    fun `Switch works as expected`() {
        val a = tagliatelle.resolve("/templates/switch.html.pasta").get().renderToString("a")
        val b = tagliatelle.resolve("/templates/switch.html.pasta").get().renderToString("b")
        val c = tagliatelle.resolve("/templates/switch.html.pasta").get().renderToString("c")

        assertTrue { test.basicallyEqual(a, "a") }
        assertTrue { test.basicallyEqual(b, "b") }
        assertTrue { test.basicallyEqual(c, "c") }
    }

    @Test
    fun `Extensions work`() {
        val result = tagliatelle.resolve("/templates/extended.html.pasta").get().renderToString()

        assertEquals("b", result)
    }

    @Test
    fun `HTML-style comments are ignored`() {
        val script = "<!--@ @unknownVariable -->Hello World"
        val context = TemplateCompilationContext(Template("test", null), SourceCodeInfo.forInlineCode(script), null)
        val errors = TemplateCompiler(context).compile()

        assertTrue { errors.isEmpty() }
        assertEquals("Hello World", context.template.renderToString())

    }

    @Test
    fun `JS-style comments are ignored`() {
        val script = "/**@ @unknownVariable */Hello World"
        val context = TemplateCompilationContext(Template("test", null), SourceCodeInfo.forInlineCode(script), null)
        val errors = TemplateCompiler(context).compile()

        assertTrue { errors.isEmpty() }
        assertEquals("Hello World", context.template.renderToString())

    }

    @Test
    fun `dynamicInvoke works`() {
        val expectedResult = resources.resolve("templates/dynamic-invoke.html").get().contentAsString
        val result = tagliatelle.resolve("/templates/dynamic-invoke-outer.html.pasta").get().renderToString()

        assertTrue { test.basicallyEqual(result, expectedResult) }
    }

    @Test
    fun `renderToString works`() {
        val expectedResult = resources.resolve("templates/render-to-string.html").get().contentAsString
        val result = tagliatelle.resolve("/templates/render-to-string.html.pasta").get().renderToString()

        assertTrue { test.basicallyEqual(result, expectedResult) }
    }

    @Test
    fun `define and xml and json macros works`() {
        val expectedResult = resources.resolve("templates/define.html").get().contentAsString
        val result = tagliatelle.resolve("/templates/define.html.pasta").get().renderToString()

        assertTrue { test.basicallyEqual(result, expectedResult) }
    }

    @Test
    fun `generateId macro works within tag libs`() {
        val expectedResult = resources.resolve("templates/generate-id.html").get().contentAsString
        val result = tagliatelle.resolve("/templates/generate-id.html.pasta").get().renderToString()

        assertTrue { test.basicallyEqual(result, expectedResult) }
    }

    @Test
    fun `Missing tag detection works`() {
        val errors = mutableListOf<CompileError>()
        try {
            tagliatelle.resolve("/templates/missing-tag.html.pasta").get()
        } catch (exception: CompileException) {
            errors.addAll(exception.errors)
        }

        assertEquals(2, errors.size)
        assertTrue { errors[0].toString().contains("Cannot find a template for the tag: w:unknown") }
        assertTrue { errors[1].toString().contains("Cannot find a handler for the internal tag: i:unknown") }
    }

    private fun compile(templateName: String): TemplateCompilationContext {
        val resource = resources.resolve(templateName).orElse(null)
        val compilationContext = tagliatelle.createResourceCompilationContext(templateName, resource, null)
        val compiler = TemplateCompiler(compilationContext)
        compiler.compile()
        return compilationContext
    }

    @Test
    fun `Argument deprecation is detected`() {
        val compilationContext = compile("/templates/deprecatedArgument.html.pasta")
        val errors = compilationContext.errors

        assertEquals(1, errors.size)
        assertEquals(ParseError.Severity.WARNING, errors[0].severity)
        assertTrue { errors[0].message.contains("The attribute 'deprecatedArg' is deprecated: Do not use") }
    }

    @Test
    fun `Duplicate arguments are detected`() {
        val compilationContext = compile("/templates/duplicateArgument.html.pasta")
        val errors = compilationContext.errors

        assertEquals(1, errors.size)
        assertEquals(ParseError.Severity.WARNING, errors[0].severity)
        assertTrue { errors[0].message.contains("An argument with the name 'test' is already defined") }
    }

    @Test
    fun `Deprecation is detected`() {
        val compilationContext = compile("/templates/deprecatedCaller.html.pasta")
        val errors = compilationContext.errors

        assertEquals(2, errors.size)
        assertEquals(ParseError.Severity.WARNING, errors[0].severity)
        assertTrue { errors[0].message.contains("The template '<e:deprecated>' is deprecated: Test of deprecated") }
    }

    @Test
    fun `Deprecation warning is skipped for deprecated templates`() {
        val compilationContext = compile("/templates/deprecatedDeprecatedCaller.html.pasta")
        val errors = compilationContext.errors

        assertTrue { errors.isEmpty() }
    }

    @Test
    fun `Calling a deprecated method is detected`() {
        val compilationContext = compile("/templates/deprecatedMethodCall.html.pasta")
        val errors = compilationContext.errors

        assertEquals(1, errors.size)
        assertEquals(ParseError.Severity.WARNING, errors[0].severity)
        assertTrue { errors[0].message.contains("The method sirius.pasta.tagliatelle.TestObject.deprecatedMethod is marked as deprecated") }
    }

    @Test
    fun `Calling a deprecated macro is detected`() {
        val compilationContext = compile("/templates/deprecatedMacroCall.html.pasta")
        val errors = compilationContext.errors

        assertEquals(1, errors.size)
        assertEquals(ParseError.Severity.WARNING, errors[0].severity)
        assertTrue { errors[0].message.contains("The macro deprecatedMacro (sirius.pasta.tagliatelle.DeprecatedMacro) is deprecated.") }
    }

    @Test
    fun `Invalid varargs are detected`() {
        val errors = mutableListOf<CompileError>()
        try {
            tagliatelle.resolve("/templates/invalid-argument-caller.html.pasta").get()
        } catch (exception: CompileException) {
            errors.addAll(exception.errors)
        }

        assertEquals(2, errors.size)
        assertEquals(ParseError.Severity.ERROR, errors[0].error.severity)
        assertTrue {
            errors[0].toString()
                    .contains("Incompatible attribute types. e:invalidArgumentTaglib expects int for 'invalidArgument', but class java.lang.String was given.")
        }
        assertFalse { errors[1].toString().contains("NullPointerException") }
    }

    @Test
    fun `Time macros state work as expected`() {
        val expectedResult = resources.resolve("templates/timeMacros.html").get().contentAsString
        val result = tagliatelle.resolve("/templates/timeMacros.html.pasta").get().renderToString()

        assertTrue { test.basicallyEqual(result, expectedResult) }
    }

    @Test
    fun `Different macro call syntax`() {
        val expectedResult = resources.resolve("templates/macroSyntax.html").get().contentAsString
        val result = tagliatelle.resolve("/templates/macroSyntax.html.pasta").get().renderToString()

        assertTrue { test.basicallyEqual(result, expectedResult) }
    }

    @Test
    fun `jsTemplate taglib works and prevents XSS`() {
        val expectedResult = resources.resolve("templates/js-template.html").get().contentAsString
        val result = tagliatelle.resolve("/templates/js-template.html.pasta").get().renderToString()

        assertTrue { test.basicallyEqual(result, expectedResult) }
    }


    @Test
    fun `Attribute expressions work`() {
        val expectedResult = resources.resolve("templates/attribute-expressions.html").get().contentAsString
        val result = tagliatelle.resolve("/templates/attribute-expressions.html.pasta").get().renderToString()

        assertTrue { test.basicallyEqual(result, expectedResult) }
    }

    @Test
    fun `Validate recursion of a template`() {
        val expectedResult = resources.resolve("templates/recursion.html").get().contentAsString
        val result = tagliatelle.resolve("/templates/recursion.html.pasta").get().renderToString()

        assertTrue { test.basicallyEqual(result, expectedResult) }
    }

    /**
     * Previously, a non-breaking whitespace (U00A0) lead to an endless loop creating an infinite number of errors.
     */
    @Test
    fun `Horror whitespaces don't crash the compiler`() {
        val script = "<i:invoke\u00A0template=\"/templates/attribute-expressions.html.pasta\"/>"
        val context = TemplateCompilationContext(Template("test", null), SourceCodeInfo.forInlineCode(script), null)
        val errors = TemplateCompiler(context).compile()

        assertEquals(1, errors.size)
    }

    @Test
    fun `Validate invoke calling original template from customization`() {
        val expectedResult = resources.resolve("templates/invoke-customized.html").get().contentAsString
        val result = tagliatelle.resolve("/templates/invoke-customized.html.pasta").get().renderToString()
        val resultCached = tagliatelle.resolve("/templates/invoke-customized.html.pasta").get().renderToString()

        assertTrue { test.basicallyEqual(result, expectedResult) }
        assertTrue { test.basicallyEqual(resultCached, expectedResult) }
    }

    @Test
    fun `Casting to inner class works`() {
        val innerClass = InnerClassTestObject.InnerClass()
        innerClass.test = "test"
        val result = tagliatelle.resolve("/templates/inner-class.html.pasta").get()
                .renderToString(Tuple.create(innerClass, innerClass))

        assertTrue { test.basicallyEqual(result, "test") }
    }

    @Test
    fun `Toplevel block and extraBlock (even when nested) produce the appropriate extra outputs`() {
        val source =
                "<i:block name=\"test\">" + "Test" + "<i:extraBlock name=\"extra-test\">Extra Test</i:extraBlock>" + "</i:block>"
        val context = TemplateCompilationContext(Template("test", null), SourceCodeInfo.forInlineCode(source), null)
        val errors = TemplateCompiler(context).compile()
        val globalRenderContext = tagliatelle.createRenderContext()
        context.template.render(globalRenderContext)

        assertTrue { errors.isEmpty() }
        // A top level i:block is expected to render its contents into an extra block
        assertEquals("Test", globalRenderContext.getExtraBlock("test"))
        // An i:extraBlock is expected to do the same - independently of its nesting and location
        assertEquals("Extra Test", globalRenderContext.getExtraBlock("extra-test"))
    }

    @Test
    fun `Minimal leading and trailing whitespace is trimmed`() {
        val source = "<i:arg type=\"String\" name=\"test\" />\n" + "\n" + "<i>@test</i>\n"
        val context = TemplateCompilationContext(
                Template("test.html.pasta", null),
                SourceCodeInfo.forInlineCode(source),
                null
        )
        val errors = TemplateCompiler(context).compile()

        assertTrue { errors.isEmpty() }
        assertEquals("<i>hello</i>", context.template.renderToString("hello"))
    }

    @Test
    fun `Whitespace trimming maintains non-minimal whitespace that has likely been placed intentionally`() {
        val source = "<i:arg type=\"String\" name=\"test\" />\n" + "\n" + "<i>@test</i>\n" + "\n"
        val context = TemplateCompilationContext(
                Template("test.html.pasta", null),
                SourceCodeInfo.forInlineCode(source),
                null
        )
        val errors = TemplateCompiler(context).compile()

        assertTrue { errors.isEmpty() }
        // of the two trailing newlines, only one is considered to be there for a (git-related) reason
        assertEquals("<i>hello</i>\n", context.template.renderToString("hello"))
    }

    @Test
    fun `Different non-emitting tags do not disturb whitespace trimming`() {
        val source =
                "<i:arg type=\"String\" name=\"test\" />\n" + "<i:local name=\"temp\" value=\"'This is a test.'\"/>\n" + "<i:local name=\"ghost\" value=\"'I am a ghost!'\"/>\n" + "\n" + "<i:pragma name=\"description\" value=\"Provides a test case for whitespace trimming\"/>\n" + "\n" + "<i>@test – @temp</i>\n"
        val context = TemplateCompilationContext(
                Template("test.html.pasta", null),
                SourceCodeInfo.forInlineCode(source),
                null
        )
        val errors = TemplateCompiler(context).compile()

        assertTrue { errors.isEmpty() }
        assertEquals("\n<i>hello – This is a test.</i>", context.template.renderToString("hello"))
    }

    companion object {
        @JvmStatic
        @Part
        private lateinit var tagliatelle: Tagliatelle

        @JvmStatic
        @Part
        private lateinit var resources: Resources

        @JvmStatic
        @Part
        private lateinit var test: TestHelper
    }
}
