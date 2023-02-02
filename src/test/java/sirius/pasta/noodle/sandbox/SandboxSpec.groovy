/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.sandbox

import sirius.kernel.BaseSpecification
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

class SandboxSpec extends BaseSpecification {

    @Part
    private static Sandbox sandbox;

    def "methods with GRANTED annotation are allowed"() {
        expect:
        sandbox.canInvoke(SandboxExample.class.getMethod("grantedMethod"))
    }

    def "methods without annotation are forbidden"() {
        expect:
        !sandbox.canInvoke(SandboxExample.class.getMethod("noAnnotation"))
        and:
        !sandbox.canInvoke(SandboxExample.class.getMethod("noAnnotation2"))
    }

    def "overridden methods with GRANTED annotation are allowed"() {
        expect:
        sandbox.canInvoke(SandboxExample2.class.getMethod("grantedMethod"))
    }

    def "overridden methods without annotation are forbidden"() {
        expect:
        !sandbox.canInvoke(SandboxExample2.class.getMethod("noAnnotation"))
    }

    def "methods without annotation overridden with GRANTED annotations are allowed"() {
        expect:
        sandbox.canInvoke(SandboxExample2.class.getMethod("noAnnotation2"))
    }

    def "methods of classes with GRANTED annotation are allowed"() {
        expect:
        sandbox.canInvoke(SandboxExample3.class.getMethod("noAnnotation"))
    }

    def "methods with REJECTED annotation are forbidden"() {
        expect:
        !sandbox.canInvoke(SandboxExample3.class.getMethod("noAnnotation2"))
    }

    def "methods whitelisted via config are allowed"() {
        expect:
        sandbox.canInvoke(CallContext.class.getMethod("getLanguage"))
        and:
        sandbox.canInvoke(Tuple.class.getMethod("getSecond"))
    }

    def "methods blocked via config are forbidden"() {
        expect:
        !sandbox.canInvoke(Tuple.class.getMethod("setSecond", Object))
    }

    def "overridden methods whitelisted via config are allowed"() {
        expect:
        sandbox.canInvoke(WebContext.class.getMethod("toString"))
        and:
        sandbox.canInvoke(ScopeInfo.class.getMethod("is", Class))
        and:
        sandbox.canInvoke(SandboxExample.class.getMethod("getName"))
        and:
        sandbox.canInvoke(SandboxExample2.class.getMethod("getName"))
        and:
        sandbox.canInvoke(SandboxExample3.class.getMethod("getName"))
    }

    def "macros with GRANTED annotation are allowed"() {
        when:
        def code = "@i18n('NLS.back')"
        def compilationContext = new TemplateCompilationContext(new Template("test", null),
                                                                SourceCodeInfo
                                                                        .forInlineCode(code, SandboxMode.WARN_ONLY),
                                                                null)
        def errors = new TemplateCompiler(compilationContext).compile()
        then:
        errors.isEmpty()
    }

    def "macros without GRANTED annotation are forbidden"() {
        when:
        def code = "@base64Resource('/assets/test.png')"
        def compilationContext = new TemplateCompilationContext(new Template("test", null),
                                                                SourceCodeInfo
                                                                        .forInlineCode(code, SandboxMode.WARN_ONLY),
                                                                null)
        def errors = new TemplateCompiler(compilationContext).compile()
        then:
        errors.size() == 1
        and:
        errors.get(0).getError().getSeverity() == ParseError.Severity.WARNING
        and:
        errors.get(0).getError().getMessage().contains("sandbox restrictions")
    }
}
