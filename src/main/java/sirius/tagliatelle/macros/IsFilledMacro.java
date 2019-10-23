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
import sirius.tagliatelle.compiler.CompilationContext;
import sirius.tagliatelle.expression.Expression;
import sirius.tagliatelle.rendering.LocalRenderContext;

import javax.annotation.Nonnull;
import java.util.List;

/**
 * Represents <tt>isFilled(Object)</tt> which is a call to {@link Strings#isFilled(Object)}.
 */
@Register
public class IsFilledMacro implements Macro {

    @Override
    public Class<?> getType() {
        return boolean.class;
    }

    @Override
    public void verifyArguments(CompilationContext context, Position pos, List<Expression> args) {
        if (args.size() != 1) {
            throw new IllegalArgumentException("Expected a single String as argument.");
        }
    }

    @Override
    public boolean isConstant(Expression[] args) {
        return true;
    }

    @Override
    public Object eval(LocalRenderContext ctx, Expression[] args) {
        return Strings.isFilled(args[0].eval(ctx));
    }

    @Nonnull
    @Override
    public String getName() {
        return "isFilled";
    }

    @Override
    public String getDescription() {
        return "Returns true, if a non empty string is given as parameter, false otherwise.";
    }
}
