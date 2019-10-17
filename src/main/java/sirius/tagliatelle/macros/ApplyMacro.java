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
 * Formats the given pattern string with the given arguments.
 *
 * @see sirius.kernel.commons.Strings#apply(String, Object...)
 */
@Register
public class ApplyMacro implements Macro {
    @Override
    public Class<?> getType() {
        return String.class;
    }

    @Override
    public void verifyArguments(CompilationContext context, Position pos, List<Expression> args) {
        if (args.isEmpty() || !Tagliatelle.isAssignableTo(args.get(0).getType(), String.class)) {
            throw new IllegalArgumentException("Expected at least a String as first argument.");
        }
    }

    @Override
    public Object eval(LocalRenderContext ctx, Expression[] args) {
        Object[] parameters = new Object[args.length - 1];
        for (int i = 1; i < args.length; i++) {
            parameters[i - 1] = args[i].eval(ctx);
        }

        return Strings.apply((String) args[0].eval(ctx), parameters);
    }

    @Override
    public boolean isConstant(Expression[] args) {
        return true;
    }

    @Nonnull
    @Override
    public String getName() {
        return "apply";
    }

    @Override
    public String getDescription() {
        return "Generates a string based on the pattern and the additionally given parameters.";
    }
}
