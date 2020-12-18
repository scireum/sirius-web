/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.tagliatelle.compiler;

import parsii.tokenizer.LookaheadReader;
import sirius.kernel.di.std.Register;
import sirius.pasta.noodle.compiler.VariableScoper;
import sirius.pasta.tagliatelle.emitter.Emitter;
import sirius.pasta.tagliatelle.emitter.LoopEmitter;

/**
 * Handles <tt>{@literal @}for(Type variable : expression) {}</tt>.
 *
 * @see LoopEmitter
 */
@Register(classes = ExpressionHandler.class)
public class ForHandler extends ExpressionHandler {

    @Override
    public boolean shouldProcess(TemplateCompiler compiler) {
        return compiler.isAtText(0, "for") && compiler.getReader().next(3).is(' ', '(');
    }

    @Override
    public Emitter process(TemplateCompiler compiler) {
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
        VariableScoper.Scope scope = compiler.getContext().getVariableScoper().pushScope();
        result.setLocalIndex(compiler.getContext()
                                     .getVariableScoper()
                                     .defineVariable(result.getStartOfBlock(), variable, type, type)
                                     .getLocalIndex());
        result.setLoop(compiler.parseBlock(null, "}"));
        scope.pop();
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
