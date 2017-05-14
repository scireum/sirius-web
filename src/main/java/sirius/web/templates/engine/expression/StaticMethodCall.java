/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates.engine.expression;

import sirius.kernel.health.Exceptions;
import sirius.web.templates.engine.CompilationContext;
import sirius.web.templates.engine.RenderContext;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.List;

/**
 * Created by aha on 10.05.17.
 */
public class StaticMethodCall extends Expression {

    private Method method;
    private Expression[] parameterExpressions;

    @Override
    public Object eval(RenderContext ctx) {
        try {
            if (parameterExpressions == null) {
                return method.invoke(null);
            }
            Object[] params = new Object[parameterExpressions.length];
            for(int i = 0; i < parameterExpressions.length; i++) {
                params[i] = parameterExpressions[i].eval(ctx);
            }

            return method.invoke(null, params);
        } catch (IllegalAccessException | InvocationTargetException e) {
            //TODO
            throw Exceptions.handle(e);
        }
    }

    @Override
    public Class<?> getType() {
        return method.getReturnType();
    }

    public void setParameters(List<Expression> parameters) {

    }

    public void bindToMethod(CompilationContext context, String methodName) {

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
        return method.getName() + "(" + sb + ")";
    }
}
