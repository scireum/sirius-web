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
import sirius.tagliatelle.Tagliatelle;
import sirius.tagliatelle.compiler.CompilationContext;
import sirius.tagliatelle.rendering.LocalRenderContext;

import java.lang.reflect.Array;
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
    public Expression propagateVisitor(ExpressionVisitor visitor) {
        this.selfExpression = selfExpression.propagateVisitor(visitor);

        return super.propagateVisitor(visitor);
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

            // Calculates if the called method has a varargs Parameter or not
            int varargsIndex = method.getParameterCount() - 1;
            boolean isVarargsMethod = method.getParameterTypes()[varargsIndex].isArray();
            Object[] params = new Object[method.getParameterCount()];

            Object varargsParam = parameterExpressions[varargsIndex].eval(ctx);
            Class<?> varargsParamClass = varargsParam.getClass();
            boolean isAlreadyVaragsArray = isVarargsMethod
                                           && varargsParamClass.isArray()
                                           && varargsParamClass.getComponentType()
                                              == method.getParameterTypes()[varargsIndex].getComponentType();

            if (isVarargsMethod && !isAlreadyVaragsArray) {
                // Fills all non-varargs parameters
                for (int i = 0; i < varargsIndex; i++) {
                    params[i] = parameterExpressions[i].eval(ctx);
                }
                // Instantiates the varargs array of the correct type
                Object[] varargs =
                        (Object[]) Array.newInstance(method.getParameterTypes()[varargsIndex].getComponentType(),
                                                     parameterExpressions.length - varargsIndex);
                // Fills the varargs parameter array
                for (int j = varargsIndex; j < parameterExpressions.length; j++) {
                    if (j == varargsIndex) {
                        varargs[0] = varargsParam;
                    } else {
                        varargs[j - varargsIndex] = parameterExpressions[j].eval(ctx);
                    }
                }
                params[varargsIndex] = varargs;
            } else {
                for (int i = 0; i < parameterExpressions.length; i++) {
                    if (i == varargsIndex) {
                        params[i] = varargsParam;
                    } else {
                        params[i] = parameterExpressions[i].eval(ctx);
                    }
                }
            }

            return method.invoke(self, params);
        } catch (IllegalArgumentException | IllegalAccessException e) {
            throw new ExpressionEvaluationException(e);
        } catch (InvocationTargetException e) {
            throw new ExpressionEvaluationException(e.getTargetException());
        }
    }

    @Override
    public Class<?> getType() {
        if (method == null) {
            return void.class;
        }

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
                if (checkSandbox(m)) {
                    return m;
                }
            }
        }

        throw new NoSuchMethodException(name);
    }

    /**
     * Ensures that no "evil" methods (reflection land) can be invoked via the template.
     * <p>
     * This way the "security" of a template can be controlled via its parameters. As static methods and <tt>new</tt>
     * cannot be invoked, most if the critical stuff is blocked anyway.
     *
     * @param method the method to check
     * @return <tt>true</tt> if the method my be invoked, <tt>false</tt> otherwise
     */
    private boolean checkSandbox(Method method) {
        if (Class.class.equals(method.getDeclaringClass())) {
            // Only getName may be invoked on a class object, no reflection stuff...
            return "getName".equals(method.getName());
        }

        return true;
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
            if (i >= method.getParameterCount() || !Tagliatelle.isAssignableTo(parameterType,
                                                                               method.getParameterTypes()[i])) {
                if (varargType == null || !Tagliatelle.isAssignableTo(parameterType, varargType)) {
                    return false;
                }
            }
        }

        return ensureEnoughParameters(method, parameterTypes, varargType);
    }

    private boolean ensureEnoughParameters(Method method, Class<?>[] parameterTypes, Class<?> varargType) {
        // The method accepts all given parameters, now ensure, that we also provide enough parameters for the method...
        if (varargType == null) {
            // No varargs -> parameters must match exactly...
            if (method.getParameterTypes().length != parameterTypes.length) {
                return false;
            }
        } else {
            // Varary -> we can at most skip the last parameter...
            if (parameterTypes.length < method.getParameterTypes().length - 1) {
                return false;
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
        if (method == null) {
            return selfExpression + ".UNBOUND(" + sb + ")";
        }
        return selfExpression + "." + method.getName() + "(" + sb + ")";
    }
}
