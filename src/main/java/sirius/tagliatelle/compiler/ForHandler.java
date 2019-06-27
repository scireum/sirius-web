/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.compiler;

import parsii.tokenizer.LookaheadReader;
import sirius.kernel.di.std.Register;
import sirius.tagliatelle.emitter.Emitter;
import sirius.tagliatelle.emitter.LoopEmitter;

/**
 * Handles <tt>{@literal @}for(Type variable : expression) {}</tt>.
 *
 * @see LoopEmitter
 */
@Register(classes = ExpressionHandler.class)
public class ForHandler extends ExpressionHandler {

    @Override
    public boolean shouldProcess(Compiler compiler) {
        return compiler.isAtText(0, "for") && compiler.getReader().next(3).is(' ', '(');
    }

    @Override
    public Emitter process(Compiler compiler) {
        LoopEmitter result = new LoopEmitter(compiler.getReader().current());
        compiler.getReader().consume(3);
        compiler.skipWhitespaces();
        compiler.consumeExpectedCharacter('(');
        compiler.skipWhitespaces();
        Class<?> type =
                compiler.getContext().resolveClass(result.getStartOfBlock(), parseTypeName(compiler.getReader()));
        compiler.skipWhitespaces();
        String variable = parseIdentifierName(compiler.getReader());
        compiler.skipWhitespaces();
        compiler.consumeExpectedCharacter(':');
        compiler.skipWhitespaces();

        result.setIterableExpression(compiler.parseExpression(true));
        compiler.skipWhitespaces();
        compiler.consumeExpectedCharacter(')');
        compiler.skipWhitespaces();
        compiler.consumeExpectedCharacter('{');
        int baseline = compiler.getContext().getVisibleStackDepth();
        result.setLocalIndex(compiler.getContext().push(result.getStartOfBlock(), variable, type));
        result.setLoop(compiler.parseBlock(null, "}"));
        compiler.getContext().popUntil(compiler.getReader().current(),baseline);
        compiler.consumeExpectedCharacter('}');

        result.verify(compiler.getContext());

        return result;
    }

    private String parseTypeName(LookaheadReader reader) {
        StringBuilder sb = new StringBuilder();
        while (reader.current().isDigit() || reader.current().isLetter() || reader.current().is('.', '_')) {
            sb.append(reader.consume().getValue());
        }
        return sb.toString();
    }

    private String parseIdentifierName(LookaheadReader reader) {
        StringBuilder sb = new StringBuilder();
        while (reader.current().isDigit() || reader.current().isLetter() || reader.current().is('_')) {
            sb.append(reader.consume().getValue());
        }
        return sb.toString();
    }
}
