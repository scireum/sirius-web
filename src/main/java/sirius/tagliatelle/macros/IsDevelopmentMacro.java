/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.macros;

import parsii.tokenizer.Position;
import sirius.kernel.Sirius;
import sirius.kernel.di.std.Register;
import sirius.tagliatelle.compiler.CompilationContext;
import sirius.tagliatelle.expression.Expression;
import sirius.tagliatelle.rendering.LocalRenderContext;

import javax.annotation.Nonnull;
import java.util.List;

/**
 * This macro determines if the current system environment is in development {@link sirius.kernel.Setup.Mode}.
 */
@Register
public class IsDevelopmentMacro implements Macro {
    @Override
    public Class<?> getType() {
        return boolean.class;
    }

    @Override
    public void verifyArguments(CompilationContext context, Position pos, List<Expression> args) {
        if (!args.isEmpty()) {
            throw new IllegalArgumentException("No arguments expected!");
        }
    }

    @Override
    public Object eval(LocalRenderContext ctx, Expression[] args) {
        return Sirius.isDev();
    }

    @Override
    public boolean isConstant(Expression[] args) {
        return false;
    }

    @Override
    public String getDescription() {
        return "Returns true if system environment in development mode, false otherwise.";
    }

    @Nonnull
    @Override
    public String getName() {
        return "isDevelopment";
    }
}
