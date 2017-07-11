package sirius.tagliatelle.macros;

import sirius.kernel.commons.Strings;
import sirius.kernel.di.std.Register;
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
    public void verifyArguments(List<Expression> args) {
        if (args.isEmpty()) {
            throw new IllegalArgumentException("At least one parameter is expected");
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
    public boolean isConstant() {
        return true;
    }

    @Nonnull
    @Override
    public String getName() {
        return "apply";
    }
}
