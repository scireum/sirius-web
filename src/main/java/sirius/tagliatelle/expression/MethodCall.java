/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.expression;

import parsii.tokenizer.Char;
import sirius.kernel.health.Exceptions;
import sirius.tagliatelle.CompilationContext;
import sirius.tagliatelle.LocalRenderContext;

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
    public Expression visit(ExpressionVisitor visitor) {
        this.selfExpression = visitor.visit(selfExpression);

        for (int i = 0; i < parameterExpressions.length; i++) {
            parameterExpressions[i] = visitor.visit(parameterExpressions[i]);
        }

        return visitor.visit(this);
    }

    @Override
    public Expression reduce() {
        this.selfExpression = selfExpression.reduce();

        for (int i = 0; i < parameterExpressions.length; i++) {
            parameterExpressions[i] = parameterExpressions[i].reduce();
        }

        return this;
    }

    @Override
    public Expression copy() {
        MethodCall copy = new MethodCall(selfExpression.copy());
        copy.method = method;
        copy.parameterExpressions = new Expression[parameterExpressions.length];
        for (int i = 0; i < parameterExpressions.length; i++) {
            copy.parameterExpressions[i] = parameterExpressions[i].copy();
        }

        return copy;
    }

    @Override
    public boolean isConstant() {
        return false;
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

    public void bindToMethod(Char position, CompilationContext context, String name) {
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
            context.error(position, "%s doesn't have a method '%s'", selfExpression.getType(), e.getMessage());
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
