/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.tagliatelle.compiler;

import sirius.kernel.di.std.Register;
import sirius.pasta.tagliatelle.emitter.Emitter;
import sirius.pasta.tagliatelle.emitter.ExpressionEmitter;

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
    public boolean shouldProcess(TemplateCompiler compiler) {
        return true;
    }

    @Override
    public Emitter process(TemplateCompiler compiler) {
        return new ExpressionEmitter(compiler.getReader().current(), compiler.parseExpression(false));
    }
}
