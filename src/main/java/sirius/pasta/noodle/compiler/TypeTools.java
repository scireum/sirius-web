/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.compiler;

import sirius.pasta.noodle.compiler.ir.Node;

import javax.annotation.Nonnull;
import java.lang.reflect.Constructor;
import java.lang.reflect.Executable;
import java.lang.reflect.GenericArrayType;
import java.lang.reflect.GenericDeclaration;
import java.lang.reflect.Method;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;
import java.lang.reflect.WildcardType;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * Provides a helper which tries to resolve all type variables of a class (and method) into concrete types.
 */
public class TypeTools {

    /**
     * A table to store type variables and their types.
     * <p>
     * The type variable names are stored scoped, i.e. <tt>java.lang.List#E</tt>, to prevent collisions when the same
     * letter is used multiple times in the inheritance tree.
     */
    private final Map<String, Type> typeTable = new LinkedHashMap<>();

    /**
     * Computes a type table for the given class.
     * <p>
     * This will first build up a type table for all type parameters for the whole inheritance tree. The resulting table
     * might look like this:
     * <table>
     * <tr><td>java.lang.List#E</td><td>String</td></tr>
     * <tr><td>java.lang.Collection#E</td><td>java.lang.List#E</td></tr>
     * <tr><td>java.lang.Iterable#T</td><td>java.lang.Collection#E</td></tr>
     * </table>
     * <p>
     * This table is then reduced by replacing the known type parameters, which results in:
     * <table>
     * <tr><td>java.lang.List#E</td><td>String</td></tr>
     * <tr><td>java.lang.Collection#E</td><td>String</td></tr>
     * <tr><td>java.lang.Iterable#T</td><td>String</td></tr>
     * </table>
     * <p>
     * If we want to determine the return type of {@link java.util.List#stream()} in our example we would look up
     * <tt>java.lang.Collection#E</tt> which results in <tt>Stream&lt;String&gt;</tt>.
     *
     * @param rootType the type to derive parameterization from
     */
    public TypeTools(Type rootType) {
        propagateParameters(rootType);
        reduceTable();
    }

    /**
     * Propagates all type parameters for the whole inheritance tree for the given type.
     *
     * @param type the type to extract the type parameters from
     */
    private void propagateParameters(Type type) {
        if (type instanceof ParameterizedType) {
            propagateParameterizedTypeParameters((ParameterizedType) type);
        }

        if (type instanceof Class<?>) {
            propagateClassParameters((Class<?>) type);
        }
    }

    /**
     * Propagates all type parameters for the whole inheritance tree for the given class.
     * <p>
     * This will propagate the superclass and the interfaces of this class.
     *
     * @param clazz the class to extract the type parameters from
     */
    private void propagateClassParameters(Class<?> clazz) {
        propagateParameters(clazz.getGenericSuperclass());

        Type[] interfaces = clazz.getGenericInterfaces();
        for (int i = 0; i < interfaces.length; i++) {
            propagateParameters(interfaces[i]);
        }
    }

    /**
     * Propagates all type parameters for the whole inheritance tree for the given parameterized type.
     * <p>
     * This will add the type parameters for the given parameterized type to our table. Afterwards the class that is
     * represented by this type will be propagated.
     *
     * @param parameterizedType the parameterized type to extract the type parameters from
     */
    private void propagateParameterizedTypeParameters(ParameterizedType parameterizedType) {
        if (!(parameterizedType.getRawType() instanceof Class<?> rawType)) {
            return;
        }

        TypeVariable<?>[] variables = rawType.getTypeParameters();
        Type[] actualParameters = parameterizedType.getActualTypeArguments();

        for (int i = 0; i < variables.length; i++) {
            addTypeVariableInfo(rawType, variables[i].getName(), actualParameters[i]);
        }

        propagateClassParameters(rawType);
    }

    /**
     * Tries to derive even more type variables by inspecting the given method and its parameters.
     * <p>
     * This essentially can derive type variables from class, object or array parameters.
     *
     * @param method     the method to fetch type variables from
     * @param parameters the parameters being passed in
     * @return the type tool itself for fluent method calls
     */
    public TypeTools withMethod(Executable method, Node[] parameters) {
        Class<?> scope = method.getDeclaringClass();

        for (TypeVariable<?> methodVariable : method.getTypeParameters()) {
            addTypeVariableInfo(scope,
                                methodVariable.getName(),
                                methodVariable.getBounds().length > 0 ? methodVariable.getBounds()[0] : Object.class);
        }

        for (int i = 0; i < method.getParameterCount() && i < parameters.length; i++) {
            Type parameterType = method.getGenericParameterTypes()[i];
            Node parameter = parameters[i];
            propagateConstantClassTypeInfos(scope, parameterType, parameter);
            propagateConstantValueTypeInfos(scope, parameterType, parameter);
            propagateConstantValueArrayTypeInfos(scope, parameterType, parameter);
        }

        reduceTable();

        return this;
    }

    private void propagateConstantClassTypeInfos(Class<?> scope, Type parameterType, Node parameter) {
        // Ensure that the parameter is of type Class<X> - abort otherwise
        if (!(parameterType instanceof ParameterizedType parameterizedType)) {
            return;
        }

        if (!(parameterizedType.getRawType() instanceof Class<?>)) {
            return;
        }

        // Read the actual type argument, this would be the X of Class<X>
        Type actualTypeArgument = parameterizedType.getActualTypeArguments()[0];

        // Abort if this isn't a type variable...
        if (!(actualTypeArgument instanceof TypeVariable)) {
            return;
        }

        String typeVariableName = ((TypeVariable<?>) actualTypeArgument).getName();

        // For a constant class, we know the resulting type...
        if (parameter.isConstant() && Class.class.isAssignableFrom(parameter.getType())) {
            addTypeVariableInfo(scope, typeVariableName, (Class<?>) parameter.getConstantValue());
        }
    }

    private void propagateConstantValueTypeInfos(Class<?> scope, Type parameterType, Node parameter) {
        // Ensure that the parameter is of type "T" (or the like) - abort otherwise
        if (!(parameterType instanceof TypeVariable)) {
            return;
        }

        String typeVariableName = ((TypeVariable<?>) parameterType).getName();
        Type genericType = parameter.getGenericType();
        Type type = genericType == null ? parameter.getType() : genericType;

        addTypeVariableInfo(scope, typeVariableName, type);
    }

    private void propagateConstantValueArrayTypeInfos(Class<?> scope, Type parameterType, Node parameter) {
        // Ensure that the parameter is of type "T" (or the like) - abort otherwise
        if (!(parameterType instanceof GenericArrayType)
            || !(((GenericArrayType) parameterType).getGenericComponentType() instanceof TypeVariable)) {
            return;
        }

        String typeVariableName =
                ((TypeVariable<?>) ((GenericArrayType) parameterType).getGenericComponentType()).getName();
        if (parameter.getType().isArray()) {
            addTypeVariableInfo(scope, typeVariableName, parameter.getType().getComponentType());
        } else {
            // seems to be a var args parameter..
            Type genericType = parameter.getGenericType();
            addTypeVariableInfo(scope, typeVariableName, genericType == null ? parameter.getType() : genericType);
        }
    }

    private void addTypeVariableInfo(Class<?> scope, String typeVariableName, Type type) {
        // Ignore void as this info is useless for us and we might even override good info
        if (type != void.class) {
            typeTable.put(getTypeVariableKey(scope, typeVariableName), type);
        }
    }

    private void reduceTable() {
        typeTable.replaceAll((typeName, type) -> simplify(type));
    }

    /**
     * Tries to simplify the given type by replacing all known type variables.
     *
     * @param type the type to simplify
     * @return the simplified type
     */
    public Type simplify(Type type) {
        if (type instanceof TypeVariable<?> typeVariable) {
            return typeTable.getOrDefault(getTypeVariableKey(getScopeOfType(typeVariable), typeVariable.getName()),
                                          type);
        }
        if ((type instanceof WildcardType wildcardType) && wildcardType.getLowerBounds().length > 0) {
            return simplify(wildcardType.getLowerBounds()[0]);
        }
        if (!(type instanceof ParameterizedType parameterizedType)) {
            return type;
        }

        if (!(parameterizedType.getRawType() instanceof Class<?> rawType)) {
            return type;
        }

        Type[] typeParameters = new Type[parameterizedType.getActualTypeArguments().length];
        for (int i = 0; i < typeParameters.length; i++) {
            typeParameters[i] = simplify(parameterizedType.getActualTypeArguments()[i]);
        }

        return new ParameterizedType() {
            @Override
            public Type[] getActualTypeArguments() {
                return typeParameters;
            }

            @Override
            public Type getRawType() {
                return rawType;
            }

            @Override
            public Type getOwnerType() {
                return null;
            }
        };
    }

    @Nonnull
    private String getTypeVariableKey(Class<?> scope, String typeVariableName) {
        return scope.getName() + "#" + typeVariableName;
    }

    /**
     * Determines the scope of the given type variable.
     *
     * @param typeVariable the type variable to determine the scope for
     * @return the class that represents the scope
     */
    private Class<?> getScopeOfType(TypeVariable<?> typeVariable) {
        GenericDeclaration genericDeclaration = typeVariable.getGenericDeclaration();

        if (genericDeclaration instanceof Executable) {
            return ((Executable) genericDeclaration).getDeclaringClass();
        }

        if (genericDeclaration instanceof Class) {
            return (Class<?>) genericDeclaration;
        }

        // Shouldn't happen as only Executable and Class are implementing GenericDeclaration in java.lang.reflect
        throw new UnsupportedOperationException("The TypeVariable " + typeVariable + " is not supported!");
    }

    /**
     * Tries to reduces the given type to a <tt>Class</tt>.
     *
     * @param type     the type to reduce
     * @param fallback used if the type cannot be reduced, e.g. for an unresolved type variable.
     * @return the reduced class or the fallback if the type cannot be reduced
     */
    public static Class<?> simplifyToClass(Type type, Class<?> fallback) {
        // Try to resolve type parameters into their actual values if possible.
        // This will propagate type parameters down a call chain.
        if (type instanceof Class<?>) {
            return (Class<?>) type;
        }
        if (type instanceof ParameterizedType) {
            return (Class<?>) ((ParameterizedType) type).getRawType();
        }

        return fallback;
    }
}
