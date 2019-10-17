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
 * Pads the given string with the given arguments.
 *
 * @see sirius.kernel.commons.Strings#rightPad(String, String, int)
 */
@Register
public class RightPadMacro implements Macro {
    @Override
    public Class<?> getType() {
        return String.class;
    }

    @Override
    public void verifyArguments(CompilationContext context, Position pos, List<Expression> args) {
        if (args.size() != 3
            || !Tagliatelle.isAssignableTo(args.get(0).getType(), String.class)
            || !Tagliatelle.isAssignableTo(args.get(1).getType(), String.class)
            || !Tagliatelle.isAssignableTo(args.get(2).getType(), int.class)) {
            throw new IllegalArgumentException("Expected the first and second argument to be a string "
                                               + "and the third to be a integer.");
        }
    }

    @Override
    public Object eval(LocalRenderContext ctx, Expression[] args) {
        return Strings.rightPad((String) args[0].eval(ctx), (String) args[1].eval(ctx), (int) args[2].eval(ctx));
    }

    @Override
    public boolean isConstant(Expression[] args) {
        return true;
    }

    @Override
    public String getDescription() {
        return "Right pads the given string.";
    }

    @Nonnull
    @Override
    public String getName() {
        return "rightPad";
    }
}
