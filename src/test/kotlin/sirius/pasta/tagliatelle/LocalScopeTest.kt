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
import sirius.kernel.di.std.Part
import sirius.kernel.tokenizer.ParseError
import sirius.pasta.noodle.compiler.CompileError
import sirius.pasta.noodle.compiler.CompileException
import sirius.pasta.tagliatelle.compiler.TemplateCompiler
import sirius.web.resources.Resources
import kotlin.test.assertEquals
import kotlin.test.assertTrue

/**
 * Tests different local scope scenarios in templates.
 */
@ExtendWith(SiriusExtension::class)
class LocalScopeTest {

    @Test
    fun `Locals within loops cleanup works`() {
        val list = listOf("a", "b", "c")
        val expectedResult = resources.resolve("templates/local-scope.html").get().contentAsString
        val context = tagliatelle.createResourceCompilationContext(
                "test",
                resources.resolve("/templates/local-scope.html.pasta").get(),
                null
        )
        val errors = TemplateCompiler(context).compile()

        assertTrue { errors.isEmpty() }
        assertTrue { test.basicallyEqual(context.template.renderToString(list), expectedResult) }
    }

    @Test
    fun `Failing access out of scope for loops and blocks works`() {
        val errors = mutableListOf<CompileError>()

        try {
            val context = tagliatelle.createResourceCompilationContext(
                    "test",
                    resources.resolve("/templates/out-of-scope.html.pasta").get(),
                    null
            )
            TemplateCompiler(context).compile()
        } catch (exception: CompileException) {
            errors.addAll(exception.errors)
        }

        assertEquals(5, errors.size)
        errors[0].apply {
            assertEquals(ParseError.Severity.ERROR, error.severity)
            assertTrue { toString().contains("Unknown variable test") }
        }
        errors[1].apply {
            assertEquals(ParseError.Severity.ERROR, error.severity)
            assertTrue { toString().contains("Unknown variable el") }
        }
        errors[2].apply {
            assertEquals(ParseError.Severity.ERROR, error.severity)
            assertTrue { toString().contains("Unknown variable test") }
        }
        errors[3].apply {
            assertEquals(ParseError.Severity.ERROR, error.severity)
            assertTrue { toString().contains("Unknown variable el") }
        }
        errors[4].apply {
            assertEquals(ParseError.Severity.ERROR, error.severity)
            assertTrue { toString().contains("Unknown variable test") }
        }
    }

    @Test
    fun `Failing access out of scope for if works`() {
        val errors = mutableListOf<CompileError>()
        try {
            val source = "<i:if test=\"1 == 1\"><i:local name=\"test\" value=\"1\"/></i:if>@test " +
                    "@if(1 == 1){<i:local name=\"test\" value=\"1\"/>} @test"
            val context = tagliatelle.createInlineCompilationContext("test", source, null)
            TemplateCompiler(context).compile()
        } catch (exception: CompileException) {
            errors.addAll(exception.errors)
        }

        assertEquals(2, errors.size)
        errors[0].apply {
            assertEquals(ParseError.Severity.ERROR, error.severity)
            assertTrue { toString().contains("Unknown variable test") }
        }
        errors[1].apply {
            assertEquals(ParseError.Severity.ERROR, error.severity)
            assertTrue { toString().contains("Unknown variable test") }
        }
    }

    @Test
    fun `Failing access out of scope for else works`() {
        val errors = mutableListOf<CompileError>()
        try {
            val source =
                    "<i:if test=\"1 == 1\"><i:else><i:local name=\"test\" value=\"1\"/></i:else></i:if>@test"
            val context = tagliatelle.createInlineCompilationContext("test", source, null)
            TemplateCompiler(context).compile()
        } catch (exception: CompileException) {
            errors.addAll(exception.errors)
        }

        assertEquals(1, errors.size)
        errors[0].apply {
            assertEquals(ParseError.Severity.ERROR, error.severity)
            assertTrue { toString().contains("Unknown variable test") }
        }
    }

    @Test
    fun `Failing access out of scope for render works`() {
        val errors = mutableListOf<CompileError>()
        try {
            val source = "<e:scope>@outer</e:scope>"
            val context = tagliatelle.createInlineCompilationContext("test", source, null)
            TemplateCompiler(context).compile()
        } catch (exception: CompileException) {
            errors.addAll(exception.errors)
        }

        assertEquals(1, errors.size)
        errors[0].apply {
            assertEquals(ParseError.Severity.ERROR, error.severity)
            assertTrue { toString().contains("Unknown variable outer") }
        }
    }

    @Test
    fun `Failing access out of scope for invoke works`() {
        val errors = mutableListOf<CompileError>()
        try {
            val source = "<i:invoke template=\"/templates/invoke-scope.html.pasta\"/>@invokeLocal"
            val context = tagliatelle.createInlineCompilationContext("test", source, null)
            TemplateCompiler(context).compile()
        } catch (exception: CompileException) {
            errors.addAll(exception.errors)
        }

        assertEquals(1, errors.size)
        errors[0].apply {
            assertEquals(ParseError.Severity.ERROR, error.severity)
            assertTrue { toString().contains("Unknown variable invokeLocal") }
        }
    }

    @Test
    fun `Failing access out of scope for include works`() {
        val errors = mutableListOf<CompileError>()
        try {
            val source = "<i:include name=\"/templates/invoke-scope.html.pasta\"/>@invokeLocal"
            val context = tagliatelle.createInlineCompilationContext("test", source, null)
            TemplateCompiler(context).compile()
        } catch (exception: CompileException) {
            errors.addAll(exception.errors)
        }

        assertEquals(2, errors.size)
        errors[1].apply {
            assertEquals(ParseError.Severity.ERROR, error.severity)
            assertTrue { toString().contains("Unknown variable invokeLocal") }
        }
    }

    @Test
    fun `Failing access out of scope for if else works`() {
        val errors = mutableListOf<CompileError>()
        try {
            val source =
                    "<i:if test=\"true\"><i:local name=\"test\" value=\"1\"/><i:else>@test</i:else></i:if>" +
                            "@if (true){<i:local name=\"test\" value=\"1\"/>}else{@test}"
            val context = tagliatelle.createInlineCompilationContext("test", source, null)
            TemplateCompiler(context).compile()
        } catch (exception: CompileException) {
            errors.addAll(exception.errors)
        }

        assertEquals(2, errors.size)
        errors[0].apply {
            assertEquals(ParseError.Severity.ERROR, error.severity)
            assertTrue { toString().contains("Unknown variable test") }
        }
        errors[1].apply {
            assertEquals(ParseError.Severity.ERROR, error.severity)
            assertTrue { toString().contains("Unknown variable test") }
        }
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
