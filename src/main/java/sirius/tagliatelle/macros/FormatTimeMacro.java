/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.macros;

import sirius.kernel.commons.Value;
import sirius.kernel.di.std.Register;
import sirius.kernel.nls.NLS;
import sirius.tagliatelle.Tagliatelle;
import sirius.tagliatelle.expression.Expression;
import sirius.tagliatelle.rendering.LocalRenderContext;

import javax.annotation.Nonnull;
import java.time.LocalTime;
import java.time.temporal.TemporalAccessor;
import java.util.Date;
import java.util.List;

/**
 * Represents <tt>formatTime(…)</tt>.
 *
 * The macro supports the datatypes {@code long} for epoch milliseconds, {@link Date} for legacy applications, and
 * {@link TemporalAccessor}. The invocation is forwarded to {@link NLS#toUserString(Object)} via
 * {@link Value#asLocalTime(LocalTime)}.
 */
@Register
public class FormatTimeMacro implements Macro {
    @Override
    public Class<?> getType() {
        return String.class;
    }

    @Override
    public void verifyArguments(List<Expression> args) {
        if (args.size() != 1) {
            throw new IllegalArgumentException("One parameter is expected");
        }

        if (!Tagliatelle.isAssignableTo(args.get(0).getType(), Long.class) &&
            !Tagliatelle.isAssignableTo(args.get(0).getType(), Date.class) &&
            !Tagliatelle.isAssignableTo(args.get(0).getType(), TemporalAccessor.class)) {
            throw new IllegalArgumentException("Illegal parameter type");
        }
    }

    @Override
    public Object eval(LocalRenderContext ctx, Expression[] args) {
        Value evalResult = Value.of(args[0].eval(ctx));
        if (evalResult.isNull()) {
            return "";
        }
        return NLS.toUserString(evalResult.asLocalTime(null));
    }

    @Override
    public boolean isConstant(Expression[] args) {
        return true;
    }

    @Nonnull
    @Override
    public String getName() {
        return "formatTime";
    }

    @Override
    public String getDescription() {
        return "Formats the given parameter as time.";
    }
}
