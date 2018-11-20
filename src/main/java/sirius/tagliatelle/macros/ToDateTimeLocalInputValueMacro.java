/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.macros;

import sirius.kernel.di.std.Register;
import sirius.tagliatelle.Tagliatelle;
import sirius.tagliatelle.expression.Expression;
import sirius.tagliatelle.rendering.LocalRenderContext;

import javax.annotation.Nonnull;
import java.time.format.DateTimeFormatter;
import java.time.temporal.Temporal;
import java.util.List;

/**
 * Formats the given parameter as a value string to be inserted into an {@code <input type="datetime-local">}.
 */
@Register
public class ToDateTimeLocalInputValueMacro implements Macro {
    @Override
    public Class<?> getType() {
        return String.class;
    }

    @Override
    public void verifyArguments(List<Expression> args) {
        if (args.size() != 1 || !Tagliatelle.isAssignableTo(args.get(0).getType(), Temporal.class)) {
            throw new IllegalArgumentException("One parameter of type Temporal is expected.");
        }
    }

    @Override
    public Object eval(LocalRenderContext ctx, Expression[] args) {
        Object temporal = args[0].eval(ctx);
        if (temporal == null) {
            return "";
        } else {
            return DateTimeFormatter.ISO_LOCAL_DATE_TIME.format((Temporal) temporal);
        }
    }

    @Override
    public boolean isConstant(Expression[] args) {
        return true;
    }

    @Nonnull
    @Override
    public String getName() {
        return "toDateTimeLocalInputValue";
    }

    @Override
    public String getDescription() {
        return "Formats the given parameter as a value string to be inserted into an <input type=\"datetime-local\">.";
    }
}
