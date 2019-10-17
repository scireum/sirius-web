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
import sirius.kernel.commons.Tuple;
import sirius.kernel.di.std.Register;
import sirius.tagliatelle.Tagliatelle;
import sirius.tagliatelle.compiler.CompilationContext;
import sirius.tagliatelle.expression.Expression;
import sirius.tagliatelle.rendering.LocalRenderContext;

import javax.annotation.Nonnull;
import java.util.List;

/**
 * Splits the given {@link String} by the first occurence of the given separator.
 *
 * @see sirius.kernel.commons.Strings#split(String, String)
 */
@Register
public class SplitMacro implements Macro {

    @Override
    public Class<?> getType() {
        return Tuple.class;
    }

    @Override
    public void verifyArguments(CompilationContext context, Position pos, List<Expression> args) {
        if (args.size() != 2
            || !Tagliatelle.isAssignableTo(args.get(0).getType(), String.class)
            || !Tagliatelle.isAssignableTo(args.get(1).getType(), String.class)) {
            throw new IllegalArgumentException("Expected two strings as arguments.");
        }
    }

    @Override
    public Object eval(LocalRenderContext ctx, Expression[] args) {
        return Strings.split((String) args[0].eval(ctx), (String) args[1].eval(ctx));
    }

    @Override
    public boolean isConstant(Expression[] args) {
        return true;
    }

    @Override
    public String getDescription() {
        return "Splits the given string on the first occurence of the given separator.";
    }

    @Nonnull
    @Override
    public String getName() {
        return "split";
    }
}
