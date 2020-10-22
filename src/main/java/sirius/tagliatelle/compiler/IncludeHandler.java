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
import sirius.tagliatelle.emitter.ConstantEmitter;
import sirius.tagliatelle.emitter.Emitter;
import sirius.tagliatelle.expression.Expression;
import sirius.web.resources.Resource;
import sirius.web.resources.Resources;

import java.util.Optional;

/**
 * Handles <tt>{@literal @}include(template)</tt>.
 * <p>
 * In contrast to {@link InvokeHandler} this will only include the given asset as literal content without any
 * processing.
 * <p>
 * If the contents being included need further processing (e.g. to include a HTML template for mustache:
 * {@code @escapeJS(inlineResource(...)} the {@link sirius.tagliatelle.macros.InlineResourceMacro} should be used.
 *
 * @see sirius.tagliatelle.emitter.InvokeTemplateEmitter
 * @see InvokeHandler
 * @see sirius.tagliatelle.macros.InlineResourceMacro
 * @see sirius.tagliatelle.macros.EscapeJSMacro
 */
@Register(classes = ExpressionHandler.class)
public class IncludeHandler extends ExpressionHandler {

    @Part
    private Resources resources;

    @Override
    public boolean shouldProcess(Compiler compiler) {
        return compiler.isAtText(0, "include") && compiler.getReader().next(7).is(' ', '(');
    }

    @Override
    public Emitter process(Compiler compiler) {
        Position position = compiler.getReader().current();
        compiler.getReader().consume(7);
        compiler.skipWhitespaces();
        compiler.consumeExpectedCharacter('(');
        Expression expression = compiler.parseExpression(true);
        compiler.skipWhitespaces();
        compiler.consumeExpectedCharacter(')');

        if (!expression.isConstant()) {
            compiler.getContext().error(position, "A constant expression is required as parameter for include!");
            return ConstantEmitter.EMPTY;
        }

        if (!Tagliatelle.isAssignableTo(expression.getType(), String.class)) {
            compiler.getContext().error(position, "A constant String is required as parameter for include!");
            return ConstantEmitter.EMPTY;
        }

        String resourcePath = (String) expression.eval(null);

        if (!resourcePath.startsWith("/assets/") && !resourcePath.startsWith("assets/")) {
            compiler.getContext()
                    .error(position,
                           "For security reasons only assets can be included. Invalid path: %s",
                           resourcePath);
            return ConstantEmitter.EMPTY;
        }

        Optional<Resource> resource = resources.resolve(resourcePath);
        if (!resource.isPresent()) {
            compiler.getContext().error(position, "Cannot find the resource: %s", resourcePath);
            return ConstantEmitter.EMPTY;
        }

        return new ConstantEmitter(position).append(resource.get().getContentAsString());
    }
}
