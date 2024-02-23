/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.sandbox


import org.junit.jupiter.api.Test
import org.junit.jupiter.api.extension.ExtendWith
import sirius.kernel.SiriusExtension
import sirius.kernel.async.CallContext
import sirius.kernel.commons.Tuple
import sirius.kernel.di.std.Part
import sirius.kernel.tokenizer.ParseError
import sirius.pasta.noodle.compiler.SourceCodeInfo
import sirius.pasta.tagliatelle.Template
import sirius.pasta.tagliatelle.compiler.TemplateCompilationContext
import sirius.pasta.tagliatelle.compiler.TemplateCompiler
import sirius.web.http.WebContext
import sirius.web.security.ScopeInfo
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertTrue

/**
 * Tests the Noodle [Sandbox].
 */
@ExtendWith(SiriusExtension::class)
class SandboxTest {

    @Test
    fun `Methods with GRANTED annotation are allowed`() {
        assertTrue {
            sandbox.canInvoke(SandboxExample::class.java.getMethod("grantedMethod"))
        }
    }

    @Test
    fun `Methods without annotation are forbidden`() {
        assertFalse {
            sandbox.canInvoke(SandboxExample::class.java.getMethod("noAnnotation"))
        }
        assertFalse {
            sandbox.canInvoke(SandboxExample::class.java.getMethod("noAnnotation2"))
        }
    }

    @Test
    fun `Overridden methods with GRANTED annotation are allowed`() {
        assertTrue {
            sandbox.canInvoke(SandboxExample2::class.java.getMethod("grantedMethod"))
        }
    }

    @Test
    fun `Overridden methods without annotation are forbidden`() {
        assertFalse {
            sandbox.canInvoke(SandboxExample2::class.java.getMethod("noAnnotation"))
        }
    }

    @Test
    fun `Methods without annotation overridden with GRANTED annotations are allowed`() {
        assertTrue {
            sandbox.canInvoke(SandboxExample2::class.java.getMethod("noAnnotation2"))
        }
    }

    @Test
    fun `Methods of classes with GRANTED annotation are allowed`() {
        assertTrue {
            sandbox.canInvoke(SandboxExample3::class.java.getMethod("noAnnotation"))
        }
    }

    @Test
    fun `Methods with REJECTED annotation are forbidden`() {
        assertFalse {
            sandbox.canInvoke(SandboxExample3::class.java.getMethod("noAnnotation2"))
        }
    }

    @Test
    fun `Methods whitelisted via config are allowed`() {
        assertTrue {
            sandbox.canInvoke(CallContext::class.java.getMethod("getLanguage"))
        }
        assertTrue {
            sandbox.canInvoke(Tuple::class.java.getMethod("getSecond"))
        }
    }

    @Test
    fun `Methods blocked via config are forbidden`() {
        assertFalse {
            sandbox.canInvoke(Tuple::class.java.getMethod("setSecond", Object::class.java))
        }
    }

    @Test
    fun `Overridden methods whitelisted via config are allowed`() {
        assertTrue {
            sandbox.canInvoke(WebContext::class.java.getMethod("toString"))
        }
        assertTrue {
            sandbox.canInvoke(ScopeInfo::class.java.getMethod("is", Class::class.java))
        }
        assertTrue {
            sandbox.canInvoke(SandboxExample::class.java.getMethod("getName"))
        }
        assertTrue {
            sandbox.canInvoke(SandboxExample2::class.java.getMethod("getName"))
        }
        assertTrue {
            sandbox.canInvoke(SandboxExample3::class.java.getMethod("getName"))
        }
    }

    @Test
    fun `Macros with GRANTED annotation are allowed`() {
        val code = "@i18n('NLS.back')"
        val compilationContext = TemplateCompilationContext(
                Template("test", null),
                SourceCodeInfo.forInlineCode(code, SandboxMode.WARN_ONLY),
                null
        )
        val errors = TemplateCompiler(compilationContext).compile()

        assertTrue {
            errors.isEmpty()
        }
    }

    @Test
    fun `Macros without GRANTED annotation are forbidden`() {
        val code = "@base64Resource('/assets/test.png')"
        val compilationContext = TemplateCompilationContext(
                Template("test", null),
                SourceCodeInfo.forInlineCode(code, SandboxMode.WARN_ONLY),
                null
        )
        val errors = TemplateCompiler(compilationContext).compile()

        assertEquals(1, errors.size)
        errors[0].error.let {
            assertEquals(ParseError.Severity.WARNING, it.severity)
            assertTrue {
                it.message.contains("sandbox restrictions")
            }
        }
    }

    @Test
    fun `Static assets included via inlineResource are allowed`() {
        val code = "@escapeJS(inlineResource('/assets/test.css'))"
        val compilationContext = TemplateCompilationContext(
                Template("test", null),
                SourceCodeInfo.forInlineCode(code, SandboxMode.WARN_ONLY),
                null
        )
        val errors = TemplateCompiler(compilationContext).compile()
        assertTrue {
            errors.isEmpty()
        }
    }

    @Test
    fun `Tagliatelle files included via inlineResource are forbidden`() {
        val code = "@escapeJS(inlineResource('/assets/test/test.js.pasta'))"
        val compilationContext = TemplateCompilationContext(
                Template("test", null),
                SourceCodeInfo.forInlineCode(code, SandboxMode.WARN_ONLY),
                null
        )
        val errors = TemplateCompiler(compilationContext).compile()
        assertEquals(1, errors.size)
        errors[0].error.let {
            assertEquals(ParseError.Severity.WARNING, it.severity)
            assertTrue {
                it.message.contains("sandbox restrictions")
            }
        }
    }

    companion object {
        @JvmStatic
        @Part
        private lateinit var sandbox: Sandbox
    }
}
