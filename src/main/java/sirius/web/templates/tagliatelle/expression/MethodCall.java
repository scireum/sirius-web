/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates.tagliatelle.expression;

import sirius.kernel.health.Exceptions;
import sirius.web.templates.tagliatelle.LocalRenderContext;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.List;

/**
 * Created by aha on 10.05.17.
 */
public class MethodCall extends Expression {

    private Method method;
    private Expression selfExpression;
    private Expression[] parameterExpressions;

    public MethodCall(Expression self) {
        this.selfExpression = self;
    }

    @Override
    public Object eval(LocalRenderContext ctx) {
        try {
            Object self = selfExpression.eval(ctx);
            if (self == null) {
                return null;
            }

            if (parameterExpressions == null) {
                return method.invoke(self);
            }

            Object[] params = new Object[parameterExpressions.length];
            for (int i = 0; i < parameterExpressions.length; i++) {
                params[i] = parameterExpressions[i].eval(ctx);
            }

            return method.invoke(self, params);
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
        if (!parameters.isEmpty()) {
            parameterExpressions = parameters.toArray(new Expression[parameters.size()]);
        }
    }

    public void bindToMethod(String name) {
        try {
            if (parameterExpressions == null) {
                this.method = selfExpression.getType().getMethod(name);
                return;
            }
            Class<?>[] parameterTypes = new Class<?>[parameterExpressions.length];
            for (int i = 0; i < parameterExpressions.length; i++) {
                parameterTypes[i] = parameterExpressions[i].getType();
            }
            this.method = selfExpression.getType().getMethod(name, parameterTypes);
        } catch (NoSuchMethodException e) {
            //TODO
            throw Exceptions.handle(e);
        }
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
        return selfExpression + "." + method.getName() + "(" + sb + ")";
    }
}
