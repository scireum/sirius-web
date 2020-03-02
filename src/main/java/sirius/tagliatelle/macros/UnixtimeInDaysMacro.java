/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.macros;

import parsii.tokenizer.Position;
import sirius.kernel.di.std.Register;
import sirius.tagliatelle.compiler.CompilationContext;
import sirius.tagliatelle.expression.Expression;
import sirius.tagliatelle.rendering.LocalRenderContext;

import javax.annotation.Nonnull;
import java.util.List;
import java.util.concurrent.TimeUnit;

/**
 * Gets the current time since 1970 in days
 */
@Register
public class UnixtimeInDaysMacro implements Macro {

    @Override
    public Class<?> getType() {
        return String.class;
    }

    @Override
    public void verifyArguments(CompilationContext context, Position position, List<Expression> args) {
        if (!args.isEmpty()) {
            throw new IllegalArgumentException("'unixtimeInDays' does not take arguments.");
        }
    }

    @Override
    public Object eval(LocalRenderContext ctx, Expression[] args) {
        return TimeUnit.DAYS.convert(System.currentTimeMillis(), TimeUnit.MILLISECONDS);
    }

    @Override
    public boolean isConstant(Expression[] args) {
        return false;
    }

    @Nonnull
    @Override
    public String getName() {
        return "unixtimeInDays";
    }

    @Override
    public String getDescription() {
        return "Gets the current time since 1970 in days";
    }
}
