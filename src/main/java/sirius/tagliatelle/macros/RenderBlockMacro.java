/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.macros;

import parsii.tokenizer.Position;
import sirius.kernel.di.std.Register;
import sirius.tagliatelle.Tagliatelle;
import sirius.tagliatelle.compiler.CompilationContext;
import sirius.tagliatelle.expression.Expression;
import sirius.tagliatelle.rendering.LocalRenderContext;

import javax.annotation.Nonnull;
import java.util.List;

/**
 * Returns the contents of the given block as string.
 */
@Register
public class RenderBlockMacro implements Macro {

    @Override
    public Class<?> getType() {
        return String.class;
    }

    @Override
    public void verifyArguments(CompilationContext context, Position pos, List<Expression> args) {
        if (args.size() != 1 || !Tagliatelle.isAssignableTo(args.get(0).getType(), String.class) || !args.get(0)
                                                                                                         .isConstant()) {
            throw new IllegalArgumentException("Expected a single String as argument.");
        }
    }

    @Override
    public Object eval(LocalRenderContext ctx, Expression[] args) {
        return ctx.getGlobalContext().emitToString(() -> {
            ctx.emitBlock((String) args[0].eval(null));
        });
    }

    @Override
    public boolean isConstant(Expression[] args) {
        return false;
    }

    @Nonnull
    @Override
    public String getName() {
        return "renderToString";
    }

    @Override
    public String getDescription() {
        return "Renders the block with the given name into a string.";
    }
}
