/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.tagliatelle.compiler;

import parsii.tokenizer.Position;
import sirius.kernel.di.std.Register;
import sirius.pasta.noodle.Callable;
import sirius.pasta.tagliatelle.emitter.AttributeExpressionEmitter;
import sirius.pasta.tagliatelle.emitter.Emitter;

/**
 * Parses an attribute expression.
 * <p>
 * This is replaces {@literal @}if (x) { y="y" } with {@literal @}y="x".
 */
@Register(classes = ExpressionHandler.class)
public class AttributeExpressionHandler extends ExpressionHandler {

    @Override
    public int getPriority() {
        return 900;
    }

    @Override
    public boolean shouldProcess(TemplateCompiler compiler) {
        int offset = 0;
        while (compiler.getReader().next(offset).isLetter()) {
            offset++;
        }
        return compiler.getReader().next(offset).is('=') && compiler.getReader().next(offset + 1).is('"');
    }

    @Override
    public Emitter process(TemplateCompiler compiler) {
        Position start = compiler.getReader().current();
        String attributeName = readAttributeName(compiler);

        compiler.consumeExpectedCharacter('=');
        compiler.consumeExpectedCharacter('"');
        compiler.skipWhitespaces();
        if (compiler.getReader().current().is('@')) {
            compiler.getReader().consume();
            compiler.skipWhitespaces();
        }

        Callable condition = compiler.parseExpression(true);
        compiler.skipWhitespaces();
        compiler.consumeExpectedCharacter('"');

        return new AttributeExpressionEmitter(start, attributeName, condition);
    }

    private String readAttributeName(TemplateCompiler compiler) {
        StringBuilder attributeName = new StringBuilder();
        while (compiler.getReader().current().isLetter()) {
            attributeName.append(compiler.getReader().consume());
        }
        return attributeName.toString();
    }
}
