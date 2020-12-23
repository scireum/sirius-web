/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.tagliatelle.compiler;

import sirius.kernel.di.std.Register;
import sirius.pasta.noodle.compiler.VariableScoper;
import sirius.pasta.tagliatelle.emitter.ConditionalEmitter;
import sirius.pasta.tagliatelle.emitter.Emitter;

/**
 * Handles <tt>{@literal @}if(expression) {} else {}</tt>.
 *
 * @see ConditionalEmitter
 */
@Register(classes = ExpressionHandler.class)
public class IfHandler extends ExpressionHandler {

    @Override
    public boolean shouldProcess(TemplateCompiler compiler) {
        return compiler.isAtText(0, "if") && compiler.getReader().next(2).is(' ', '(');
    }

    @Override
    public Emitter process(TemplateCompiler compiler) {
        ConditionalEmitter result = new ConditionalEmitter(compiler.getReader().current());
        compiler.getReader().consume(2);
        compiler.skipWhitespaces();
        compiler.consumeExpectedCharacter('(');
        result.setConditionExpression(compiler.parseExpression(true));
        compiler.skipWhitespaces();
        compiler.consumeExpectedCharacter(')');
        compiler.skipWhitespaces();
        compiler.consumeExpectedCharacter('{');
        VariableScoper.Scope scope = compiler.getContext().getVariableScoper().pushScope();
        result.setWhenTrue(compiler.parseBlock(null, "}"));
        compiler.consumeExpectedCharacter('}');
        compiler.skipWhitespaces();
        if (isAtElse(compiler)) {
            scope.pop();
            compiler.skipWhitespaces();
            compiler.getReader().consume(4);
            compiler.skipWhitespaces();
            compiler.consumeExpectedCharacter('{');
            result.setWhenFalse(compiler.parseBlock(null, "}"));
            compiler.consumeExpectedCharacter('}');
        }
        scope.pop();

        result.verify(compiler.getContext());

        return result;
    }

    private boolean isAtElse(TemplateCompiler compiler) {
        int offset = 0;
        while (compiler.getReader().next(offset).isWhitepace()) {
            offset++;
        }
        return compiler.isAtText(offset, "else");
    }
}
