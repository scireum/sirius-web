/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.macros;

import sirius.kernel.di.std.Register;
import sirius.tagliatelle.expression.Expression;
import sirius.tagliatelle.rendering.LocalRenderContext;

import javax.annotation.Nonnull;
import java.time.LocalDateTime;
import java.time.LocalTime;

/**
 * Macro for comparing it a given date is before another given date or now.
 */
@Register(classes = Macro.class)
public class IsBeforeMacro extends DateComparingMacro {

    @Override
    public Object eval(LocalRenderContext ctx, Expression[] args) {
        LocalDateTime firstDate = parseInput(args[0].eval(ctx));

        LocalDateTime secondDate;
        if (args.length == 2) {
            secondDate = parseInput(args[1].eval(ctx));
        } else {
            secondDate = LocalDateTime.now();
        }

        if (LocalTime.MIDNIGHT.equals(firstDate.toLocalTime())) {
            return firstDate.toLocalDate().isBefore(secondDate.toLocalDate());
        }
        return firstDate.isBefore(secondDate);
    }

    @Override
    public boolean isConstant(Expression[] args) {
        return true;
    }

    @Override
    public String getDescription() {
        return "Compares if the first given date / date with time is before the second given date / date with time or the curent date / date with time if no second argument was given.";
    }

    @Nonnull
    @Override
    public String getName() {
        return "isBefore";
    }
}
