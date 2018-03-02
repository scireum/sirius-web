/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle;

import sirius.kernel.di.std.Register;
import sirius.kernel.health.Counter;
import sirius.tagliatelle.expression.Expression;
import sirius.tagliatelle.macros.Macro;
import sirius.tagliatelle.rendering.LocalRenderContext;

import javax.annotation.Nonnull;
import java.util.List;

/**
 * Returns a {@link Counter} which can be used to ensure, that inlining doesn't re-evaluate expressions.
 */
@Register
public class CounterMacro implements Macro {
    @Override
    public Class<?> getType() {
        return Counter.class;
    }

    @Override
    public void verifyArguments(List<Expression> args) {

    }

    @Override
    public Object eval(LocalRenderContext ctx, Expression[] args) {
        return new Counter();
    }

    @Override
    public boolean isConstant(Expression[] args) {
        return false;
    }

    @Override
    public String getDescription() {
        return null;
    }

    @Nonnull
    @Override
    public String getName() {
        return "counter";
    }
}
