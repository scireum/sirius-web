/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.macros;

import sirius.kernel.di.std.Register;
import sirius.kernel.nls.NLS;
import sirius.tagliatelle.Tagliatelle;
import sirius.tagliatelle.expression.Expression;
import sirius.tagliatelle.rendering.LocalRenderContext;

import javax.annotation.Nonnull;
import java.time.temporal.Temporal;
import java.util.List;

/**
 * Represents <tt>toSpokenDate(Temporal)</tt> which is a call to {@link NLS#toSpokenDate(Temporal)}.
 */
@Register
public class ToSpokenDateMacro implements Macro {
    @Override
    public Class<?> getType() {
        return String.class;
    }

    @Override
    public void verifyArguments(List<Expression> args) {
        if (args.size() != 1 || !Tagliatelle.isAssignableTo(args.get(0).getType(), Temporal.class)) {
            throw new IllegalArgumentException("Expected a single Temporal argument");
        }
    }

    @Override
    public Object eval(LocalRenderContext ctx, Expression[] args) {
        return NLS.toSpokenDate((Temporal) args[0].eval(ctx));
    }

    @Override
    public boolean isConstant(Expression[] args) {
        return false;
    }

    @Nonnull
    @Override
    public String getName() {
        return "toSpokenDate";
    }

    @Override
    public String getDescription() {
        return "Converts dates to a \"human\" (e.g. \"today\", \"yesterday\") format.";
    }
}
