/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.expression;

import parsii.tokenizer.Char;
import sirius.kernel.commons.Strings;
import sirius.tagliatelle.Engine;
import sirius.tagliatelle.compiler.CompilationContext;
import sirius.tagliatelle.rendering.LocalRenderContext;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

/**
 * Invokes a Java Method.
 */
public class MethodCall extends Call {

    private Method method;
    private Expression selfExpression;

    /**
     * Creates a new instance and specifies the expression on which the method is invoked.
     *
     * @param self the expression on which the method is invoked.
     */
    public MethodCall(Expression self) {
        this.selfExpression = self;
    }

    @Override
    public Expression visit(ExpressionVisitor visitor) {
        this.selfExpression = visitor.visit(selfExpression);

        return super.visit(visitor);
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
        copyParametersTo(copy);

        return copy;
    }

    @Override
    public Object eval(LocalRenderContext ctx) {
        try {
            Object self = selfExpression.eval(ctx);
            if (self == null) {
                return null;
            }

            if (parameterExpressions == NO_ARGS) {
                return method.invoke(self);
            }

            Object[] params = new Object[parameterExpressions.length];
            for (int i = 0; i < parameterExpressions.length; i++) {
                params[i] = parameterExpressions[i].eval(ctx);
            }

            return method.invoke(self, params);
        } catch (IllegalAccessException e) {
            throw new ExpressionEvaluationException(e);
        } catch (InvocationTargetException e) {
            throw new ExpressionEvaluationException(e.getTargetException());
        }
    }

    @Override
    public Class<?> getType() {
        return method.getReturnType();
    }

    /**
     * Tries to find a matching method for the given name, type of "self" and parameter types.
     *
     * @param position the position where the invocation was declared
     * @param context  the compilation context for error reporting
     * @param name     the name of the method to find
     */
    public void bindToMethod(Char position, CompilationContext context, String name) {
        try {
            if (parameterExpressions == NO_ARGS) {
                this.method = selfExpression.getType().getMethod(name);
                return;
            }
            Class<?>[] parameterTypes = new Class<?>[parameterExpressions.length];
            for (int i = 0; i < parameterExpressions.length; i++) {
                parameterTypes[i] = parameterExpressions[i].getType();
            }

            this.method = findMethod(selfExpression.getType(), name, parameterTypes);
        } catch (NoSuchMethodException e) {
            context.error(position, "%s doesn't have a method '%s'", selfExpression.getType(), e.getMessage());
        }
    }

    private Method findMethod(Class<?> type, String name, Class<?>[] parameterTypes) throws NoSuchMethodException {
        for (Method m : type.getMethods()) {
            if (signatureMatch(m, name, parameterTypes)) {
                return m;
            }
        }

        throw new NoSuchMethodException(name);
    }

    private boolean signatureMatch(Method method, String name, Class<?>[] parameterTypes) {
        if (!Strings.areEqual(name, method.getName())) {
            return false;
        }

        Class<?> varargType = null;
        for (int i = 0; i < parameterTypes.length; i++) {
            Class<?> parameterType = parameterTypes[i];
            if (i == method.getParameterCount() - 1 && method.getParameterTypes()[i].isArray()) {
                varargType = method.getParameterTypes()[i].getComponentType();
            }
            if (i >= method.getParameterCount() || !Engine.isAssignableTo(parameterType,
                                                                          method.getParameterTypes()[i])) {
                if (varargType == null || !Engine.isAssignableTo(parameterType, varargType)) {
                    return false;
                }
            }
        }

        return true;
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
