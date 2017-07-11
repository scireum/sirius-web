package sirius.tagliatelle.macros;

import sirius.kernel.di.std.Register;
import sirius.kernel.nls.NLS;
import sirius.tagliatelle.expression.Expression;
import sirius.tagliatelle.rendering.LocalRenderContext;

import javax.annotation.Nonnull;
import java.util.List;

/**
 * Represents <tt>toMachineString(Object)</tt> which is a call to {@link sirius.kernel.nls.NLS#toMachineString(Object)}.
 */
@Register
public class ToMachineStringMacro implements Macro {
    @Override
    public Class<?> getType() {
        return String.class;
    }

    @Override
    public void verifyArguments(List<Expression> args) {
        if (args.size() != 1) {
            throw new IllegalArgumentException("1 parameter expected");
        }
    }

    @Override
    public Object eval(LocalRenderContext ctx, Expression[] args) {
        return NLS.toMachineString(args[0].eval(ctx));
    }

    @Override
    public boolean isConstant() {
        return true;
    }

    @Nonnull
    @Override
    public String getName() {
        return "toMachineString";
    }
}
