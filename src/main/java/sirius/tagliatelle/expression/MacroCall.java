/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.expression;

import sirius.kernel.di.GlobalContext;
import sirius.kernel.di.std.Part;
import sirius.tagliatelle.CompilationContext;
import sirius.tagliatelle.LocalRenderContext;
import sirius.tagliatelle.macros.Macro;

import java.util.List;

/**
 * Created by aha on 10.05.17.
 */
public class MacroCall extends Expression {

    public static final Expression[] NO_ARGS = {};
    private Expression[] parameterExpressions = NO_ARGS;
    private Macro macro;

    @Part
    private static GlobalContext ctx;

    @Override
    public Object eval(LocalRenderContext ctx) {
        return macro.eval(ctx, parameterExpressions);
    }

    @Override
    public Class<?> getType() {
        return macro.getType();
    }

    public void setParameters(List<Expression> parameters) {
        if (parameters != null && !parameters.isEmpty()) {
            this.parameterExpressions = parameters.toArray(new Expression[parameters.size()]);
        }
    }

    public void bindToMethod(CompilationContext context, String methodName) {
        this.macro = ctx.findPart(methodName, Macro.class);
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        if (parameterExpressions != null) {
            for (Expression expr : parameterExpressions) {
                if (sb.length() > 0) {
                    sb.append(", ");
                }
                sb.append(expr);
            }
        }
        return macro.getName() + "(" + sb + ")";
    }
}
