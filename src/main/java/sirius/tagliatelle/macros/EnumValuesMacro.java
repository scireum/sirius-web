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
import sirius.tagliatelle.expression.ConstantClass;
import sirius.tagliatelle.expression.Expression;
import sirius.tagliatelle.rendering.LocalRenderContext;

import javax.annotation.Nonnull;
import java.util.Arrays;
import java.util.List;

/**
 * Returns all values of a given enum class.
 */
@Register
public class EnumValuesMacro implements Macro {

    @Override
    public Class<?> getType() {
        return List.class;
    }

    @Override
    public void verifyArguments(CompilationContext context, Position pos, List<Expression> args) {
        if (args.size() == 1 && (args.get(0) instanceof ConstantClass)) {
            Class<?> type = (Class<?>) args.get(0).eval(null);
            if (type.isEnum()) {
                return;
            }
        }

        throw new IllegalArgumentException("Expected an enum class as parameter.");
    }

    @Override
    public Object eval(LocalRenderContext ctx, Expression[] args) {
        Class<?> type = (Class<?>) args[0].eval(ctx);
        return Arrays.asList(type.getEnumConstants());
    }

    @Override
    public boolean isConstant(Expression[] args) {
        return true;
    }

    @Nonnull
    @Override
    public String getName() {
        return "enumValues";
    }

    @Override
    public String getDescription() {
        return "Returns a list of all enum constants of the given enum class.";
    }
}
