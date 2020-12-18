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
import sirius.pasta.noodle.Callable;
import sirius.pasta.noodle.ConstantCall;
import sirius.pasta.noodle.ScriptingException;
import sirius.pasta.noodle.compiler.CompilationContext;
import sirius.pasta.tagliatelle.emitter.ConstantEmitter;
import sirius.pasta.tagliatelle.emitter.Emitter;
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
 * {@code @escapeJS(inlineResource(...)} the {@link sirius.pasta.noodle.macros.InlineResourceMacro} should be used.
 *
 * @see sirius.pasta.tagliatelle.emitter.InvokeTemplateEmitter
 * @see InvokeHandler
 * @see sirius.pasta.noodle.macros.InlineResourceMacro
 * @see sirius.pasta.noodle.macros.EscapeJSMacro
 */
@Register(classes = ExpressionHandler.class)
public class IncludeHandler extends ExpressionHandler {

    @Part
    private Resources resources;

    @Override
    public boolean shouldProcess(TemplateCompiler compiler) {
        return compiler.isAtText(0, "include") && compiler.getReader().next(7).is(' ', '(');
    }

    @Override
    public Emitter process(TemplateCompiler compiler) {
        Position position = compiler.getReader().current();
        compiler.getReader().consume(7);
        compiler.skipWhitespaces();
        compiler.consumeExpectedCharacter('(');
        Callable expression = compiler.parseExpression(true);
        compiler.skipWhitespaces();
        compiler.consumeExpectedCharacter(')');

        if (!(expression instanceof ConstantCall)) {
            compiler.getContext().error(position, "A constant expression is required as parameter for include!");
            return ConstantEmitter.EMPTY;
        }

        if (!CompilationContext.isAssignableTo(expression.getType(), String.class)) {
            compiler.getContext().error(position, "A constant String is required as parameter for include!");
            return ConstantEmitter.EMPTY;
        }

        try {
            String resourcePath = (String) expression.call(null);

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
        } catch (ScriptingException e) {
            compiler.getContext().error(position, "Failed to compute template path: %s", e.getMessage());
            return ConstantEmitter.EMPTY;
        }
    }
}
