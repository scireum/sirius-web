package sirius.tagliatelle.macros;

import sirius.kernel.di.std.Register;
import sirius.kernel.nls.Formatter;
import sirius.tagliatelle.expression.Expression;
import sirius.tagliatelle.rendering.LocalRenderContext;

import javax.annotation.Nonnull;
import java.util.List;

/**
 * Formats the given pattern string with the given arguments.
 * <p>
 * The first argument in the args array represents the format string, all other arguments are applied to it.
 * e.g. calling @apply("This is a ${1} and this a second ${2}", "formatted string", "formatted parameter") would produce the
 * string: "This is a formatted string and this is a second formatted parameter".
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
            throw new IllegalArgumentException("At least 1 parameter expected");
        }
    }

    @Override
    public Object eval(LocalRenderContext ctx, Expression[] args) {
        Formatter formatter = Formatter.create((String) args[0].eval(ctx));

        for (int i = 1; i < args.length; i++) {
            formatter.set(String.valueOf(i), args[i].eval(ctx));
        }

        return formatter.format();
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
