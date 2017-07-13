package sirius.tagliatelle.macros;

import sirius.kernel.di.std.Register;
import sirius.kernel.nls.NLS;
import sirius.tagliatelle.expression.Expression;
import sirius.tagliatelle.rendering.LocalRenderContext;

import javax.annotation.Nonnull;
import java.util.List;

/**
 * Represents <tt>toUserString(Object)</tt> which is a call to {@link NLS#toUserString(Object)}.
 */
@Register
public class ToUserStringMacro implements Macro {
    @Override
    public Class<?> getType() {
        return String.class;
    }

    @Override
    public void verifyArguments(List<Expression> args) {
        if (args.size() != 1) {
            throw new IllegalArgumentException("One parameter is expected");
        }
    }

    @Override
    public Object eval(LocalRenderContext ctx, Expression[] args) {
        return NLS.toUserString(args[0].eval(ctx));
    }

    @Override
    public boolean isConstant(Expression[] args) {
        return false;
    }

    @Nonnull
    @Override
    public String getName() {
        return "toUserString";
    }
}
