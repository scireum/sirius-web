/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.compiler;

import sirius.kernel.di.std.Register;
import sirius.tagliatelle.emitter.Emitter;
import sirius.tagliatelle.emitter.ExpressionEmitter;

/**
 * Parses an expression to be evaluated at runtime.
 */
@Register(classes = ExpressionHandler.class)
public class EvalExpressionHandler extends ExpressionHandler {

    @Override
    public int getPriority() {
        return 999;
    }

    @Override
    public boolean shouldProcess(Compiler compiler) {
        return true;
    }

    @Override
    public Emitter process(Compiler compiler) {
        compiler.getReader().consume();
        return new ExpressionEmitter(compiler.getReader().current(), compiler.parseExpression(false));
    }
}
