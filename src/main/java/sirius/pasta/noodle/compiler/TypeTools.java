/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.compiler;

import sirius.pasta.noodle.compiler.ir.Node;

import java.lang.reflect.GenericArrayType;
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

    private final Map<String, Type> typeTable = new LinkedHashMap<>();

    /**
     * Computes a type table for the given class.
     *
     * @param rootType the type to derive parameterization from
     */
    public TypeTools(Type rootType) {
        if (!(rootType instanceof ParameterizedType)) {
            return;
        }

        ParameterizedType parameterizedType = (ParameterizedType) rootType;
        if (!(parameterizedType.getRawType() instanceof Class<?>)) {
            return;
        }

        TypeVariable<?>[] variables = ((Class<?>) parameterizedType.getRawType()).getTypeParameters();
        Type[] actualParameters = parameterizedType.getActualTypeArguments();
        for (int i = 0; i < variables.length; i++) {
            typeTable.put(variables[i].getName(), actualParameters[i]);
        }

        reduceTable();
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
    public TypeTools withMethod(Method method, Node[] parameters) {
        for (TypeVariable<?> methodVariable : method.getTypeParameters()) {
            typeTable.put(methodVariable.getName(),
                          methodVariable.getBounds().length > 0 ? methodVariable.getBounds()[0] : Object.class);
        }

        for (int i = 0; i < method.getParameterCount() && i < parameters.length; i++) {
            Type parameterType = method.getGenericParameterTypes()[i];
            Node parameter = parameters[i];
            propagateConstantClassTypeInfos(parameterType, parameter);
            propagateConstantValueTypeInfos(parameterType, parameter);
            propagateConstantValueArrayTypeInfos(parameterType, parameter);
        }

        reduceTable();

        return this;
    }

    private void propagateConstantClassTypeInfos(Type parameterType, Node parameter) {
        // Ensure that the parameter is of type Class<X> - abort otherwise
        if (!(parameterType instanceof ParameterizedType)) {
            return;
        }

        ParameterizedType parameterizedType = (ParameterizedType) parameterType;

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
            typeTable.put(typeVariableName, (Class<?>) parameter.getConstantValue());
        }
    }

    private void propagateConstantValueTypeInfos(Type parameterType, Node parameter) {
        // Ensure that the parameter is of type "T" (or the like) - abort otherwise
        if (!(parameterType instanceof TypeVariable)) {
            return;
        }

        String typeVariableName = ((TypeVariable<?>) parameterType).getName();
        Type genericType = parameter.getGenericType();
        Type type = genericType == null ? parameter.getType() : genericType;

        if ("void".equals(type.getTypeName())) {
            typeTable.putIfAbsent(typeVariableName, type);
        } else {
            typeTable.put(typeVariableName, type);
        }
    }

    private void propagateConstantValueArrayTypeInfos(Type parameterType, Node parameter) {
        // Ensure that the parameter is of type "T" (or the like) - abort otherwise
        if (!(parameterType instanceof GenericArrayType)
            || !(((GenericArrayType) parameterType).getGenericComponentType() instanceof TypeVariable)) {
            return;
        }

        String typeVariableName =
                ((TypeVariable<?>) ((GenericArrayType) parameterType).getGenericComponentType()).getName();
        if (parameter.getType().isArray()) {
            typeTable.put(typeVariableName, parameter.getType().getComponentType());
        } else {
            // seems to be a var args parameter..
            Type genericType = parameter.getGenericType();
            typeTable.put(typeVariableName, genericType == null ? parameter.getType() : genericType);
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
        if (type instanceof TypeVariable<?>) {
            return typeTable.getOrDefault(((TypeVariable<?>) type).getName(), type);
        }
        if (type instanceof WildcardType) {
            if (((WildcardType) type).getLowerBounds().length > 0) {
                return simplify(((WildcardType) type).getLowerBounds()[0]);
            }
        }
        if (!(type instanceof ParameterizedType)) {
            return type;
        }

        ParameterizedType parameterizedType = (ParameterizedType) type;
        if (!(parameterizedType.getRawType() instanceof Class<?>)) {
            return type;
        }

        Class<?> rawType = (Class<?>) parameterizedType.getRawType();
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
