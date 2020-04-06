/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.expression;

import parsii.tokenizer.Char;
import sirius.kernel.commons.Explain;
import sirius.kernel.commons.Strings;
import sirius.kernel.health.Exceptions;
import sirius.tagliatelle.Tagliatelle;
import sirius.tagliatelle.compiler.CompilationContext;
import sirius.tagliatelle.rendering.LocalRenderContext;

import java.lang.reflect.Array;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;

/**
 * Invokes a Java {@link Method}.
 */
public class MethodCall extends Call {

    private Method method;
    private Class<?> varArgType;
    private int varArgIndex;
    private Expression selfExpression;

    private static final String[] EMPTY_STRING_ARRAY = new String[0];
    private static final Object[] EMPTY_OBJECT_ARRAY = new Object[0];

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

    @SuppressWarnings("ArrayEquality")
    @Explain("The same constant is always used")
    @Override
    public Object eval(LocalRenderContext ctx) {
        try {
            Object self = selfExpression.eval(ctx);
            if (self == null) {
                return null;
            }

            if (parameterExpressions == NO_ARGS && varArgType == null) {
                return method.invoke(self);
            }

            Object[] params = translateMethodParameters(ctx);
            return method.invoke(self, params);
        } catch (IllegalArgumentException | IllegalAccessException e) {
            throw new ExpressionEvaluationException(e);
        } catch (InvocationTargetException e) {
            throw new ExpressionEvaluationException(e.getTargetException());
        }
    }

    /**
     * Translates the given list of parameter expressions into an actual array objects to be passed to the method call.
     * <p>
     * This is trivial up until vararg methods are involved. In this case we might compact a list of additional
     * arguments into its own array or supply an empty array if no parameters were supplied.
     *
     * @param ctx the context used to evaluate parameter expressions
     * @return the effective array of parameter objects to pass to the method call
     */
    private Object[] translateMethodParameters(LocalRenderContext ctx) {
        Object[] params = new Object[method.getParameterCount()];
        Object[] varArgs = null;

        for (int i = 0; i < parameterExpressions.length; i++) {
            Object param = parameterExpressions[i].eval(ctx);
            if (varArgType == null || i < varArgIndex || isMatchingVarargArray(i, param)) {
                params[i] = param;
            } else {
                if (varArgs == null) {
                    varArgs = createVarArgArray();
                    params[varArgIndex] = varArgs;
                }
                varArgs[i - varArgIndex] = param;
            }
        }

        if (varArgType != null && parameterExpressions.length < method.getParameterCount()) {
            appendMissingVarArgArray(params);
        }

        return params;
    }

    /**
     * Adds an empty array as last parameter, if no vararg parameter was present at all.
     *
     * @param params the parameter list to complete
     */
    private void appendMissingVarArgArray(Object[] params) {
        if (varArgType == Object.class) {
            params[varArgIndex] = EMPTY_OBJECT_ARRAY;
        } else if (varArgType == String.class) {
            params[varArgIndex] = EMPTY_STRING_ARRAY;
        } else {
            params[varArgIndex] = Array.newInstance(varArgType, 0);
        }
    }

    private Object[] createVarArgArray() {
        return (Object[]) Array.newInstance(varArgType, parameterExpressions.length - method.getParameterCount() + 1);
    }

    /**
     * Determines if the given parameter for a given index exactly matches the parameter type required for a vararg
     * method.
     * <p>
     * Therefore the parameter must be an array of the right type and also both the last parameter of the method and the
     * last parameter in the invokation.
     *
     * @param index the current index of the parameter being processed
     * @param param the evaluated parameter to process
     * @return <tt>true</tt> if it as a matching vararg array, <tt>false</tt> if a conversion is required
     */
    private boolean isMatchingVarargArray(int index, Object param) {
        if (index != varArgIndex) {
            return false;
        }

        if (index != parameterExpressions.length - 1) {
            return false;
        }

        if (param == null) {
            return false;
        }

        if (!param.getClass().isArray()) {
            return false;
        }

        return param.getClass().getComponentType() == varArgType;
    }

    @Override
    public Class<?> getType() {
        if (method == null) {
            return void.class;
        }

        // Try to resolve type parameters into their actual values if possible.
        // This will propagate type parameters down a call chain.
        Type genericType = getGenericType();
        if (genericType instanceof Class<?>) {
            return (Class<?>) genericType;
        }
        if (genericType instanceof ParameterizedType) {
            return (Class<?>) ((ParameterizedType) genericType).getRawType();
        }

        return method.getReturnType();
    }

    @Override
    public Type getGenericType() {
        if (method.getGenericReturnType() instanceof TypeVariable) {
            Type parameterizedType = tryResolveViaParameterizedCallee();
            if (parameterizedType != null) {
                return parameterizedType;
            }

            Type constantClassType = tryResolveViaConstantClassParameter();
            if (constantClassType != null) {
                return constantClassType;
            }
        }

        return method.getGenericReturnType();
    }

    /**
     * Tries to resolve a type variable by inspecting the parameterization of the callsite.
     * <p>
     * If a method returns a generic variable like {@code ValueHolder<V>.get()} does,
     * we try to determine the generic type parameters from the class we're being invoked
     * on. If we end up at a method or variable declaration which specifies the types, we
     * can propagate this information down a call chain and therefore deduce the effective type.
     *
     * @return the type deduced from a parameterization of the "this" expression or <tt>null</tt>
     * if not enough information is present
     */
    private Type tryResolveViaParameterizedCallee() {
        Type parentType = selfExpression.getGenericType();
        if (parentType instanceof ParameterizedType) {
            int index = determineGenericIndex(method.getGenericReturnType().getTypeName(),
                                              (Class<?>) ((ParameterizedType) parentType).getRawType());
            if (index >= 0) {
                return ((ParameterizedType) parentType).getActualTypeArguments()[index];
            }
        }

        return null;
    }

    /**
     * Tries to deduce the return type of a method by looking for appropriate class parameters.
     * <p>
     * Many methods return an object of a given class like {@link sirius.web.security.UserContext#getHelper(Class)}
     * does. Therefore, if we detect a type variable as return type, we try to find a constant class parameter which
     * binds to the same type variable and try to deduce the effective return type this way.
     *
     * @return the deduced class parameter for the returned type variable or <tt>null</tt> if not enough information
     * if present
     */
    private Type tryResolveViaConstantClassParameter() {
        String variableName = ((TypeVariable<?>) method.getGenericReturnType()).getName();
        for (int i = 0; i < method.getParameterCount(); i++) {
            Type resolvedType = tryResolveAsConstantClassParameter(variableName, i);
            if (resolvedType != null) {
                return resolvedType;
            }
        }

        return null;
    }

    private Type tryResolveAsConstantClassParameter(String variableName, int parameterIndex) {
        // Ensure that the parameter is of type Class<X> - abort otherwise
        if (!(Class.class.equals(method.getParameterTypes()[parameterIndex])
              && method.getGenericParameterTypes()[parameterIndex] instanceof ParameterizedType)) {
            return null;
        }

        // Read the actual type argument, this would be the X of Class<X>
        Type actualTypeArgument =
                ((ParameterizedType) method.getGenericParameterTypes()[parameterIndex]).getActualTypeArguments()[0];

        // Abort if this isn't a type variable...
        if (!(actualTypeArgument instanceof TypeVariable)) {
            return null;
        }

        String typeVariableName = ((TypeVariable<?>) actualTypeArgument).getName();
        // Make sure that this class actually binds to our type variable and not to
        // anything else...
        if (!Strings.areEqual(variableName, typeVariableName)) {
            return null;
        }

        // For a constant class, we know the resulting type...
        if (parameterExpressions[parameterIndex] instanceof ConstantClass) {
            return (Class<?>) parameterExpressions[parameterIndex].eval(null);
        }

        // Otherwise resort to the generic type - if available...
        Type genericType = parameterExpressions[parameterIndex].getGenericType();
        if (genericType != null) {
            return genericType;
        }

        // Or finally resort to the known type...
        return parameterExpressions[parameterIndex].getType();
    }

    private int determineGenericIndex(String parameterName, Class<?> clazz) {
        int index = 0;
        for (TypeVariable<?> param : clazz.getTypeParameters()) {
            if (param.getName().equals(parameterName)) {
                return index;
            }
            index++;
        }

        return -1;
    }

    /**
     * Tries to find a matching method for the given name, type of "self" and parameter types.
     *
     * @param position the position where the invocation was declared
     * @param context  the compilation context for error reporting
     * @param name     the name of the method to find
     */
    @SuppressWarnings("ArrayEquality")
    @Explain("The same constant is always used")
    public void bindToMethod(Char position, CompilationContext context, String name) {
        if (parameterExpressions == NO_ARGS) {
            try {
                this.method = selfExpression.getType().getMethod(name);
                checkDeprecation(position, context);
                return;
            } catch (NoSuchMethodException e) {
                Exceptions.ignore(e);
            }
        }

        try {
            Class<?>[] parameterTypes = new Class<?>[parameterExpressions.length];
            for (int i = 0; i < parameterExpressions.length; i++) {
                parameterTypes[i] = parameterExpressions[i].getType();
            }

            this.method = findMethod(selfExpression.getType(), name, parameterTypes);
            checkDeprecation(position, context);
            setupVarArgs();
        } catch (NoSuchMethodException e) {
            context.error(position, "%s doesn't have a method '%s'", selfExpression.getType(), e.getMessage());
        }
    }

    private void checkDeprecation(Char position, CompilationContext context) {
        if (this.method.isAnnotationPresent(Deprecated.class)) {
            context.warning(position,
                            "The method %s.%s is marked as deprecated",
                            this.method.getDeclaringClass().getName(),
                            this.method.getName());
        }
    }

    /**
     * Pre-fills certain fields required at render time for var arg methods
     */
    private void setupVarArgs() {
        if (method.getParameterCount() == 0) {
            return;
        }

        if (!method.getParameterTypes()[method.getParameterCount() - 1].isArray()) {
            return;
        }

        varArgIndex = method.getParameterCount() - 1;
        varArgType = method.getParameterTypes()[varArgIndex].getComponentType();
    }

    private Method findMethod(Class<?> type, String name, Class<?>[] parameterTypes) throws NoSuchMethodException {
        // First try a regular lookup without coercing of any kind.
        // This is reasonable, as this takes generic type parameters
        // into account and selects the proper method...
        try {
            Method m = type.getMethod(name, parameterTypes);
            if (checkSandbox(m)) {
                return m;
            }
        } catch (NoSuchMethodException e) {
            Exceptions.ignore(e);
        }

        // Try to find an appropriate method using coercions known to the system...
        for (Method m : type.getMethods()) {
            if (signatureMatch(m, name, parameterTypes) && checkSandbox(m)) {
                return m;
            }
        }

        throw new NoSuchMethodException(name);
    }

    /**
     * Ensures that no "evil" methods (reflection land) can be invoked via the template.
     * <p>
     * This way the "security" of a template can be controlled via its parameters. As static methods and <tt>new</tt>
     * cannot be invoked, most of the critical stuff is blocked anyway.
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

        // If the method is a pure var arg method and no parameters are given at all.... this will work out...
        if (parameterTypes.length == 0 && method.getParameterCount() == 1 && method.getParameterTypes()[0].isArray()) {
            return true;
        }

        // Check all given parameters and pickup and check the var arg type if the method has one...
        if (!checkParameterTypes(method, parameterTypes)) {
            return false;
        }

        return ensureEnoughParameters(method, parameterTypes);
    }

    @SuppressWarnings("squid:S3776")
    @Explain("As this involves a stateful complex check, we rather keep all checks in one place.")
    private boolean checkParameterTypes(Method method, Class<?>[] parameterTypes) {
        Class<?> varargType = null;
        for (int i = 0; i < parameterTypes.length; i++) {
            Class<?> parameterType = parameterTypes[i];
            Class<?> methodParameterType = i < method.getParameterCount() ? method.getParameterTypes()[i] : varargType;

            // If the last parameter is an array, we have to determine its component type as this might be
            // a var-arg call...
            if (method.isVarArgs() && i == method.getParameterCount() - 1 && methodParameterType.isArray()) {
                varargType = methodParameterType.getComponentType();

                // For a var-arg parameter, the last parameter has either to be the exact same type (an array)
                // or it has to match the vararg type
                if (!Tagliatelle.isAssignableTo(parameterType, methodParameterType) && !Tagliatelle.isAssignableTo(
                        parameterType,
                        varargType)) {
                    return false;
                }

                // If we matched the original array type, we cannot support additional vararg parameters,
                // therefore the argument count must match...
                if (!Tagliatelle.isAssignableTo(parameterType, varargType)
                    && parameterTypes.length > method.getParameterCount()) {
                    return false;
                }
            } else if (methodParameterType == null || !Tagliatelle.isAssignableTo(parameterType, methodParameterType)) {
                // For all other parameters (than the last) we simply ensure, that the parameter type is assignable...
                return false;
            }
        }

        return true;
    }

    private boolean ensureEnoughParameters(Method method, Class<?>[] parameterTypes) {
        // The method accepts all given parameters, now ensure, that we also provide enough parameters for the method...
        if (!method.isVarArgs()) {
            // No varargs -> parameters must match exactly...
            return method.getParameterTypes().length == parameterTypes.length;
        } else {
            // Varary -> we can at most skip the last parameter...
            return parameterTypes.length >= method.getParameterTypes().length - 1;
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
        if (method == null) {
            return selfExpression + ".UNBOUND(" + sb + ")";
        }
        return selfExpression + "." + method.getName() + "(" + sb + ")";
    }
}
