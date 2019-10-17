/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.macros;

import parsii.tokenizer.Position;
import sirius.kernel.commons.Strings;
import sirius.kernel.di.std.Register;
import sirius.tagliatelle.Tagliatelle;
import sirius.tagliatelle.compiler.CompilationContext;
import sirius.tagliatelle.expression.Expression;
import sirius.tagliatelle.rendering.LocalRenderContext;

import javax.annotation.Nonnull;
import java.util.List;

/**
 * Represents <tt>join(Iterable, String)</tt> which is a call to {@link Strings#join(Iterable, String)}.
 */
@Register
public class JoinMacro implements Macro {
    @Override
    public Class<?> getType() {
        return String.class;
    }

    @Override
    public void verifyArguments(CompilationContext context, Position pos, List<Expression> args) {
        if (args.size() != 2
            || !Tagliatelle.isAssignableTo(args.get(0).getType(), Iterable.class)
            || !Tagliatelle.isAssignableTo(args.get(1).getType(), String.class)) {
            throw new IllegalArgumentException(
                    "Expected the first parameter to be an Iterable and the second to be a String");
        }
    }

    @Override
    public Object eval(LocalRenderContext ctx, Expression[] args) {
        return Strings.join((Iterable<?>) args[0].eval(ctx), (String) args[1].eval(ctx));
    }

    @Override
    public boolean isConstant(Expression[] args) {
        return true;
    }

    @Nonnull
    @Override
    public String getName() {
        return "join";
    }

    @Override
    public String getDescription() {
        return "Returns a string concatenation of the given lists items";
    }
}
