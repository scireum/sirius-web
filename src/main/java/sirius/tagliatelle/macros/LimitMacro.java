/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.macros;

import sirius.kernel.commons.Strings;
import sirius.kernel.di.std.Register;
import sirius.tagliatelle.Tagliatelle;
import sirius.tagliatelle.expression.Expression;
import sirius.tagliatelle.rendering.LocalRenderContext;

import javax.annotation.Nonnull;
import java.util.List;

/**
 * Limit the given object to a given lenth.
 *
 * @see sirius.kernel.commons.Strings#limit(Object, int)
 * @see sirius.kernel.commons.Strings#limit(Object, int, boolean)
 */
@Register
public class LimitMacro implements Macro {

    @Override
    public Class<?> getType() {
        return String.class;
    }

    @Override
    public void verifyArguments(List<Expression> args) {
        if (args.size() < 2 || args.size() > 3 || !Tagliatelle.isAssignableTo(args.get(1).getType(), int.class)) {
            throw new IllegalArgumentException(
                    "Expected the first argument to be an object and the second argument to be an integer.");
        }
        if (args.size() == 3 && !Tagliatelle.isAssignableTo(args.get(2).getType(), boolean.class)) {
            throw new IllegalArgumentException("Expected the third argument to be a boolean.");
        }
    }

    @Override
    public Object eval(LocalRenderContext ctx, Expression[] args) {
        if (args.length == 3) {
            return Strings.limit(args[0].eval(ctx), (int) args[1].eval(ctx), (boolean) args[2].eval(ctx));
        } else {
            return Strings.limit(args[0].eval(ctx), (int) args[1].eval(ctx));
        }
    }

    @Override
    public boolean isConstant(Expression[] args) {
        return true;
    }

    @Override
    public String getDescription() {
        return "Limits the given object.";
    }

    @Nonnull
    @Override
    public String getName() {
        return "limit";
    }
}
