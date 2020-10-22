/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.compiler;

import parsii.tokenizer.Position;
import sirius.kernel.di.std.Part;
import sirius.kernel.di.std.Register;
import sirius.tagliatelle.Tagliatelle;
import sirius.tagliatelle.Template;
import sirius.tagliatelle.emitter.ConstantEmitter;
import sirius.tagliatelle.emitter.Emitter;
import sirius.tagliatelle.expression.Expression;
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
 * @see sirius.tagliatelle.emitter.InvokeTemplateEmitter
 * @see IncludeHandler
 * @see sirius.tagliatelle.macros.InlineResourceMacro
 * @see sirius.tagliatelle.macros.EscapeJSMacro
 */
@Register(classes = ExpressionHandler.class)
public class InvokeHandler extends ExpressionHandler {

    @Part
    private Resources resources;

    @Override
    public boolean shouldProcess(Compiler compiler) {
        return compiler.isAtText(0, "invoke") && compiler.getReader().next(6).is(' ', '(');
    }

    @Override
    public Emitter process(Compiler compiler) {
        Position position = compiler.getReader().current();
        compiler.getReader().consume(6);
        compiler.skipWhitespaces();
        compiler.consumeExpectedCharacter('(');
        Expression expression = compiler.parseExpression(true);
        compiler.skipWhitespaces();
        compiler.consumeExpectedCharacter(')');

        if (!expression.isConstant()) {
            compiler.getContext().error(position, "A constant expression is required as parameter for invoke!");
            return ConstantEmitter.EMPTY;
        }

        if (!Tagliatelle.isAssignableTo(expression.getType(), String.class)) {
            compiler.getContext().error(position, "A constant String is required as parameter for invoke!");
            return ConstantEmitter.EMPTY;
        }

        String resourcePath = (String) expression.eval(null);
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
}
