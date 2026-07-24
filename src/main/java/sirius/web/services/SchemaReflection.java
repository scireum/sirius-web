/*
 * Made with all the love in the world
 * by scireum in Stuttgart, Germany
 *
 * Copyright by scireum GmbH
 * https://www.scireum.de - info@scireum.de
 */

package sirius.web.services;

import javax.annotation.Nullable;
import java.lang.reflect.GenericArrayType;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;
import java.lang.reflect.WildcardType;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

/**
 * Provides the reflection helpers used to introspect the request and response POJOs of
 * {@linkplain sirius.web.controller.Route#isMappedPayload() mapped services}.
 * <p>
 * Both the human-readable field listing ({@link SchemaFieldInfo}) and the machine-readable OpenAPI schema
 * ({@link OpenApiGenerator}) walk the same generic type graph. These helpers resolve type variables, unwrap
 * collections and maps and decide which types are treated as leaves (and are therefore not expanded any further).
 */
public final class SchemaReflection {

    private SchemaReflection() {
    }

    /**
     * Resolves type variables and wildcards against the given bindings.
     *
     * @param type          the type to resolve
     * @param typeVariables the known bindings of type variables to actual types
     * @return the resolved type, or {@link Object} if it cannot be resolved
     */
    public static Type resolveType(Type type, Map<TypeVariable<?>, Type> typeVariables) {
        if (type instanceof TypeVariable<?> typeVariable) {
            return typeVariables.getOrDefault(typeVariable, Object.class);
        }
        if (type instanceof WildcardType wildcardType) {
            Type[] upperBounds = wildcardType.getUpperBounds();
            return upperBounds.length == 1 ? resolveType(upperBounds[0], typeVariables) : Object.class;
        }
        return type;
    }

    /**
     * Resolves the bindings of the type variables declared by the given raw type against its actual type arguments.
     * <p>
     * This is used to carry generic information down while walking a type graph, e.g. to resolve the {@code T} in a
     * {@code Response<T>} to its actual type argument.
     *
     * @param type          the (possibly parameterized) type to inspect
     * @param rawType       the raw class of the given type
     * @param typeVariables the currently known bindings
     * @return a new map containing the given bindings plus the ones declared by the given type
     */
    public static Map<TypeVariable<?>, Type> resolveTypeVariables(Type type,
                                                                  Class<?> rawType,
                                                                  Map<TypeVariable<?>, Type> typeVariables) {
        Map<TypeVariable<?>, Type> nestedTypeVariables = new HashMap<>(typeVariables);
        if (type instanceof ParameterizedType parameterizedType) {
            TypeVariable<?>[] variables = rawType.getTypeParameters();
            Type[] arguments = parameterizedType.getActualTypeArguments();
            for (int index = 0; index < Math.min(variables.length, arguments.length); index++) {
                nestedTypeVariables.put(variables[index], resolveType(arguments[index], typeVariables));
            }
        }
        return nestedTypeVariables;
    }

    /**
     * Determines the raw class of the given type.
     *
     * @param type the type to inspect
     * @return the raw class, or <tt>null</tt> if it cannot be determined
     */
    @Nullable
    public static Class<?> determineRawType(Type type) {
        if (type instanceof Class<?> clazz) {
            return clazz;
        }
        if (type instanceof ParameterizedType parameterizedType
            && parameterizedType.getRawType() instanceof Class<?> rawType) {
            return rawType;
        }
        if (type instanceof GenericArrayType) {
            return Object[].class;
        }
        return null;
    }

    /**
     * Determines the element type of an array or {@link Collection} type.
     *
     * @param type the array or collection type
     * @return the element type, or {@link Object} if it cannot be determined
     */
    public static Type determineCollectionElementType(Type type) {
        if (type instanceof GenericArrayType arrayType) {
            return arrayType.getGenericComponentType();
        }
        Class<?> rawType = determineRawType(type);
        if (rawType != null && rawType.isArray()) {
            return rawType.getComponentType();
        }
        return determineTypeArgument(type, 0);
    }

    /**
     * Determines the value type of a {@link Map} type.
     *
     * @param type the map type
     * @return the value type, or {@link Object} if it cannot be determined
     */
    public static Type determineMapValueType(Type type) {
        return determineTypeArgument(type, 1);
    }

    private static Type determineTypeArgument(Type type, int index) {
        if (type instanceof ParameterizedType parameterizedType
            && parameterizedType.getActualTypeArguments().length > index) {
            return parameterizedType.getActualTypeArguments()[index];
        }
        return Object.class;
    }

    /**
     * Determines whether the given type is a leaf which must not be expanded into individual fields.
     * <p>
     * Leaves are primitives, enums, numbers, character sequences and all types from the {@code java.} packages.
     *
     * @param type the type to check
     * @return <tt>true</tt> if the type is a leaf, <tt>false</tt> otherwise
     */
    public static boolean isLeafType(Class<?> type) {
        return type.isPrimitive()
               || type.isEnum()
               || type.isArray() && isLeafType(type.getComponentType())
               || Number.class.isAssignableFrom(type)
               || CharSequence.class.isAssignableFrom(type)
               || Boolean.class.equals(type)
               || Character.class.equals(type)
               || type.getPackageName().startsWith("java.");
    }
}
