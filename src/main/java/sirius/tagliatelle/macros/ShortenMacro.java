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
 * Represents <tt>shorten(String, int)</tt> which is a call to {@link Strings#shorten(String, int)}.
 */
@Register
public class ShortenMacro implements Macro {
    @Override
    public Class<?> getType() {
        return String.class;
    }

    @Override
    public void verifyArguments(List<Expression> args) {
        if (args.size() != 2 || !Tagliatelle.isAssignableTo(args.get(0).getType(), String.class)|| !Tagliatelle.isAssignableTo(args.get(1).getType(), int.class)) {
            throw new IllegalArgumentException("Expected the first parameter to be a String and the second to be an Integer");
        }
    }

    @Override
    public Object eval(LocalRenderContext ctx, Expression[] args) {
        return Strings.shorten((String) args[0].eval(ctx), (int) args[1].eval(ctx));
    }

    @Override
    public boolean isConstant(Expression[] args) {
        return true;
    }

    @Nonnull
    @Override
    public String getName() {
        return "shorten";
    }

    @Override
    public String getDescription() {
        return "shortens a string to the given number of chars, cutting of at most half of the string and adding ... if something has been cut of.";
    }
}
