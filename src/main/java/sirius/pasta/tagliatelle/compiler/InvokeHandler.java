/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.tagliatelle.compiler;

import parsii.tokenizer.Position;
import sirius.kernel.di.std.Part;
import sirius.kernel.di.std.Register;
import sirius.kernel.health.Exceptions;
import sirius.pasta.noodle.Callable;
import sirius.pasta.noodle.ConstantCall;
import sirius.pasta.noodle.ScriptingException;
import sirius.pasta.noodle.compiler.CompilationContext;
import sirius.pasta.noodle.compiler.CompileException;
import sirius.pasta.tagliatelle.Template;
import sirius.pasta.tagliatelle.emitter.ConstantEmitter;
import sirius.pasta.tagliatelle.emitter.Emitter;
import sirius.web.resources.Resources;

import java.util.Collections;

/**
 * Handles <tt>{@literal @}invoke(template)</tt>.
 * <p>
 * This is especially useful if a JavaScript template wants to include another
 * using {@code ___invoke(other)}. Note that this does not accept any further
 * parameters for the template to be invoked.
 * <p>
 * If a plain file is to be included without further processing, {@link IncludeHandler} can be used. If a template
 * is to be included within a JavaScript string (e.g. for Mustache), {@code @escapeJS(inlineResource(...))} should
 * be used.
 *
 * @see sirius.pasta.tagliatelle.emitter.InvokeTemplateEmitter
 * @see IncludeHandler
 * @see sirius.pasta.noodle.macros.InlineResourceMacro
 * @see sirius.pasta.noodle.macros.EscapeJsMacro
 */
@Register(classes = ExpressionHandler.class)
public class InvokeHandler extends ExpressionHandler {

    @Part
    private Resources resources;

    @Override
    public boolean shouldProcess(TemplateCompiler compiler) {
        return compiler.isAtText(0, "invoke") && compiler.getReader().next(6).is(' ', '(');
    }

    @Override
    public Emitter process(TemplateCompiler compiler) {
        Position position = compiler.getReader().current();
        compiler.getReader().consume(6);
        compiler.skipWhitespaces();
        compiler.consumeExpectedCharacter('(');
        Callable expression = compiler.parseExpression(true);
        compiler.skipWhitespaces();
        compiler.consumeExpectedCharacter(')');

        if (!(expression instanceof ConstantCall)) {
            compiler.getContext().error(position, "A constant expression is required as parameter for invoke!");
            return ConstantEmitter.EMPTY;
        }

        if (!CompilationContext.isAssignableTo(expression.getType(), String.class)) {
            compiler.getContext().error(position, "A constant String is required as parameter for invoke!");
            return ConstantEmitter.EMPTY;
        }

        String resourcePath = (String) safeCall(expression);
        try {
            Template template = compiler.getContext().resolveTemplate(position, resourcePath).orElse(null);

            if (template == null) {
                compiler.getContext().error(position, "Cannot find the referenced template: %s", resourcePath);
            }

            return compiler.getContext().invokeTemplate(position, template, ignored -> null, Collections.emptyMap());
        } catch (CompileException e) {
            compiler.getContext()
                    .error(position, "Error compiling referenced template: %s%n%s", resourcePath, e.getMessage());

            return null;
        }
    }

    private Object safeCall(Callable expression) {
        try {
            return expression.call(null);
        } catch (ScriptingException e) {
            Exceptions.ignore(e);
            return null;
        }
    }
}
