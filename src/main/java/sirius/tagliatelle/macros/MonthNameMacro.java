package sirius.tagliatelle.macros;

import sirius.kernel.nls.NLS;
import sirius.tagliatelle.Tagliatelle;
import sirius.tagliatelle.expression.Expression;
import sirius.tagliatelle.rendering.LocalRenderContext;

import javax.annotation.Nonnull;
import java.util.List;

/**
 * Represents <tt>monthName(int)</tt> which is a call to {@link NLS#getMonthName(int)}.
 */
public class MonthNameMacro implements Macro {
    @Override
    public Class<?> getType() {
        return String.class;
    }

    @Override
    public void verifyArguments(List<Expression> args) {
        if (args.size() != 1 || !Tagliatelle.isAssignableTo(args.get(0).getType(), int.class)) {
            throw new IllegalArgumentException("Expected exactly one argument as an int.");
        }
    }

    @Override
    public Object eval(LocalRenderContext ctx, Expression[] args) {
        return NLS.getMonthName((Integer) args[0].eval(ctx));
    }

    @Override
    public boolean isConstant(Expression[] args) {
        return false;
    }

    @Override
    public String getDescription() {
        return "Generates the string representation of a month";
    }

    @Nonnull
    @Override
    public String getName() {
        return "monthName";
    }
}
