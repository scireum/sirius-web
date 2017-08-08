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
import java.text.DecimalFormat;
import java.util.List;

/**
 * Uses DecimalFormatter to format large numbers with dots for better readability.
 */
@Register
public class NumberFormatMacro implements Macro {

    @Override
    public Class<?> getType() {
        return String.class;
    }

    @Override
    public void verifyArguments(List<Expression> args) {
        if (args.size() != 1 || !Tagliatelle.isAssignableTo(args.get(0).getType(), Number.class)) {
            throw new IllegalArgumentException("Expected the parameter to be of a Number type.");
        }
    }

    @Override
    public Object eval(LocalRenderContext ctx, Expression[] args) {
        DecimalFormat formatter = new DecimalFormat();
        formatter.setGroupingUsed(true);
        formatter.setMaximumFractionDigits(2);
        return formatter.format(args[0].eval(ctx));
    }

    @Override
    public boolean isConstant(Expression[] args) {
        return true;
    }

    @Override
    public String getDescription() {
        return "Formats large long or int numbers with dots for better readability.";
    }

    @Nonnull
    @Override
    public String getName() {
        return "numberFormat";
    }
}
