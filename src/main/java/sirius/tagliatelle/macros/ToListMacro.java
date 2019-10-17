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
import sirius.tagliatelle.compiler.CompilationContext;
import sirius.tagliatelle.expression.Expression;
import sirius.tagliatelle.rendering.LocalRenderContext;

import javax.annotation.Nonnull;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Represents a list containing all given parameters. If only one parameter of an array type is given, it will be
 * expanded into the list instead.
 */
@Register
public class ToListMacro implements Macro {

    @Override
    public Class<?> getType() {
        return List.class;
    }

    @Override
    public void verifyArguments(CompilationContext context, Position pos, List<Expression> args) {
        if (args.isEmpty()) {
            throw new IllegalArgumentException("Expected at least one parameter");
        }
    }

    @Override
    public Object eval(LocalRenderContext ctx, Expression[] args) {
        if (args.length == 1 && args[0].getType().isArray()) {
            return Arrays.stream((Object[]) args[0].eval(ctx)).collect(Collectors.toList());
        } else {
            return Arrays.stream(args).map(element -> element.eval(ctx)).collect(Collectors.toList());
        }
    }

    @Override
    public boolean isConstant(Expression[] args) {
        return true;
    }

    @Nonnull
    @Override
    public String getName() {
        return "toList";
    }

    @Override
    public String getDescription() {
        return "Returns a list containing of the given items or the given array";
    }
}
