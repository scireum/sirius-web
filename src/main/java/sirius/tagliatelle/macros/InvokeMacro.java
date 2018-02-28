package sirius.tagliatelle.macros;

import sirius.kernel.di.std.Part;
import sirius.kernel.di.std.Register;
import sirius.kernel.health.Exceptions;
import sirius.tagliatelle.Tagliatelle;
import sirius.tagliatelle.expression.Expression;
import sirius.tagliatelle.rendering.LocalRenderContext;

import javax.annotation.Nonnull;
import java.util.List;

/**
 * Resolves the tagliatelle template defined by the first parameter of this macro.
 */
@Register
public class InvokeMacro implements Macro {

    @Part
    private static Tagliatelle tagliatelle;

    @Override
    public Class<?> getType() {
        return String.class;
    }

    @Override
    public void verifyArguments(List<Expression> args) {
        if (args.isEmpty() || !Tagliatelle.isAssignableTo(args.get(0).getType(), String.class)) {
            throw new IllegalArgumentException("Expected at least a String as first argument.");
        }
    }

    @Override
    public Object eval(LocalRenderContext ctx, Expression[] args) {
        try {
            String templateName = (String) args[0].eval(ctx);

            return tagliatelle.resolve(templateName)
                    .orElseThrow(() -> Exceptions.handle().withSystemErrorMessage("Cannot find the referenced template: " + templateName).handle())
                    .renderToString();
        } catch (Exception e) {
            throw Exceptions.handle(e);
        }
    }

    @Override
    public boolean isConstant(Expression[] args) {
        return false;
    }

    @Override
    public String getDescription() {
        return "Resolves the tagliatelle template defined by the first parameter of this macro.";
    }

    @Nonnull
    @Override
    public String getName() {
        return "invoke";
    }
}
