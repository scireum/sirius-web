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
import java.util.List;

@Register
public class ParseUserStringMacro implements Macro {

    @Override
    public Class<?> getType() {
        return Object.class;
    }

    @Override
    public void verifyArguments(List<Expression> args) {
        if (args.size() != 2
            || !Tagliatelle.isAssignable(args.get(0).getType(), Class.class)
            || !Tagliatelle.isAssignableTo(args.get(1).getType(), String.class)) {
            throw new IllegalArgumentException(
                    "Expected the first argument to be a class and the second argument to be a string");
        }
    }

    @Override
    public Object eval(LocalRenderContext ctx, Expression[] args) {
        return NLS.parseUserString((Class) args[0].eval(ctx), (String) args[1].eval(ctx));
    }

    @Override
    public boolean isConstant(Expression[] args) {
        return true;
    }

    @Override
    public String getDescription() {
        return "Parses the given user string into the desired object if possible";
    }

    @Nonnull
    @Override
    public String getName() {
        return "parseUserString";
    }
}
