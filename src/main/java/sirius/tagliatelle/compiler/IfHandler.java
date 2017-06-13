/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.compiler;

import sirius.kernel.di.std.Register;
import sirius.tagliatelle.emitter.ConditionalEmitter;
import sirius.tagliatelle.emitter.Emitter;

/**
 * Handles <tt>{@literal @}if(expression) {} else {}</tt>.
 *
 * @see ConditionalEmitter
 */
@Register(classes = ExpressionHandler.class)
public class IfHandler extends ExpressionHandler {

    @Override
    public boolean shouldProcess(Compiler compiler) {
        return compiler.isAtText(0, "@if");
    }

    @Override
    public Emitter process(Compiler compiler) {
        ConditionalEmitter result = new ConditionalEmitter(compiler.getReader().current());
        compiler.getReader().consume(3);
        compiler.skipWhitespaces();
        compiler.consumeExpectedCharacter('(');
        result.setConditionExpression(compiler.parseExpression(true));
        compiler.skipWhitespaces();
        compiler.consumeExpectedCharacter(')');
        compiler.skipWhitespaces();
        compiler.consumeExpectedCharacter('{');
        result.setWhenTrue(compiler.parseBlock(null, "}"));
        compiler.consumeExpectedCharacter('}');
        compiler.skipWhitespaces();
        if (isAtElse(compiler)) {
            compiler.skipWhitespaces();
            compiler.reader.consume(4);
            compiler.skipWhitespaces();
            compiler.consumeExpectedCharacter('{');
            result.setWhenFalse(compiler.parseBlock(null, "}"));
            compiler.consumeExpectedCharacter('}');
        }

        return result;
    }

    private boolean isAtElse(Compiler compiler) {
        int offset = 0;
        while (compiler.getReader().next(offset).isWhitepace()) {
            offset++;
        }
        return compiler.isAtText(offset, "else");
    }
}
