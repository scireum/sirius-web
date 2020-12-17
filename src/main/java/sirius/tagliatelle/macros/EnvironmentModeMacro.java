/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.macros;

import parsii.tokenizer.Position;
import sirius.tagliatelle.compiler.CompilationContext;
import sirius.tagliatelle.expression.Expression;

import java.util.List;

/**
 * Base class for environment determining {@link Macro}s.
 */
public abstract class EnvironmentModeMacro implements Macro {
    @Override
    public Class<?> getType() {
        return boolean.class;
    }

    @Override
    public void verifyArguments(CompilationContext context, Position pos, List<Expression> args) {
        if (!args.isEmpty()) {
            throw new IllegalArgumentException("No arguments expected!");
        }
    }

    @Override
    public boolean isConstant(Expression[] args) {
        return true;
    }
}
