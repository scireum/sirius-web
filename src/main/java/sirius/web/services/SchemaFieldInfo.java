/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.services;

import io.swagger.v3.oas.annotations.media.Schema;
import sirius.kernel.commons.Strings;
import sirius.kernel.nls.NLS;

import javax.annotation.Nullable;
import java.lang.reflect.Field;
import java.lang.reflect.GenericArrayType;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;
import java.lang.reflect.WildcardType;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Describes a single field of an input or output POJO of a {@linkplain PublicService mapped service}.
 * <p>
 * The information is derived from the {@link Schema} annotations placed on the fields of the respective POJO and is
 * used to automatically document {@linkplain sirius.web.controller.Route#isMappedPayload() mapped} services.
 *
 * @see PublicServiceInfo#getInputSchema()
 * @see PublicServiceInfo#getOutputSchema()
 */
public class SchemaFieldInfo {

    private final String name;
    private final String type;
    private final boolean required;
    private final String description;
    private final String example;

    private SchemaFieldInfo(String name, String type, boolean required, String description, String example) {
        this.name = name;
        this.type = type;
        this.required = required;
        this.description = description;
        this.example = example;
    }

    /**
     * Introspects the given type and derives a field description for each field which carries a {@link Schema} annotation.
     * Nested POJOs and collection elements are recursively expanded using dotted field names.
     *
     * @param type the type to introspect, may be <tt>null</tt>
     * @return the list of described fields, possibly empty
     */
    public static List<SchemaFieldInfo> forType(@Nullable Type type) {
        if (type == null) {
            return List.of();
        }

        List<SchemaFieldInfo> fields = new ArrayList<>();
        collectFields(type, "", new HashMap<>(), new HashSet<>(), fields);
        return fields;
    }

    private static void collectFields(Type type,
                                      String prefix,
                                      Map<TypeVariable<?>, Type> typeVariables,
                                      Set<Class<?>> typesOnPath,
                                      List<SchemaFieldInfo> fields) {
        Type resolvedType = resolveType(type, typeVariables);
        Class<?> rawType = determineRawType(resolvedType);
        if (rawType == null || isLeafType(rawType) || !typesOnPath.add(rawType)) {
            return;
        }

        Map<TypeVariable<?>, Type> nestedTypeVariables = new HashMap<>(typeVariables);
        if (resolvedType instanceof ParameterizedType parameterizedType) {
            TypeVariable<?>[] variables = rawType.getTypeParameters();
            Type[] arguments = parameterizedType.getActualTypeArguments();
            for (int index = 0; index < Math.min(variables.length, arguments.length); index++) {
                nestedTypeVariables.put(variables[index], resolveType(arguments[index], typeVariables));
            }
        }

        collectFields(rawType.getSuperclass(), prefix, nestedTypeVariables, typesOnPath, fields);
        for (Field field : rawType.getDeclaredFields()) {
            Schema schema = field.getAnnotation(Schema.class);
            if (schema == null) {
                continue;
            }

            Type fieldType = resolveType(field.getGenericType(), nestedTypeVariables);
            String fieldName = prefix + (Strings.isFilled(schema.name()) ? schema.name() : field.getName());
            fields.add(describeField(fieldName, fieldType, schema));
            collectNestedFields(fieldType, fieldName, nestedTypeVariables, typesOnPath, fields);
        }
        typesOnPath.remove(rawType);
    }

    private static void collectNestedFields(Type type,
                                            String fieldName,
                                            Map<TypeVariable<?>, Type> typeVariables,
                                            Set<Class<?>> typesOnPath,
                                            List<SchemaFieldInfo> fields) {
        Class<?> rawType = determineRawType(type);
        if (rawType == null) {
            return;
        }
        if (rawType.isArray() || Collection.class.isAssignableFrom(rawType)) {
            collectFields(determineCollectionElementType(type), fieldName + "[].", typeVariables, typesOnPath, fields);
        } else if (Map.class.isAssignableFrom(rawType)) {
            collectFields(determineMapValueType(type), fieldName + "[].", typeVariables, typesOnPath, fields);
        } else if (!isLeafType(rawType)) {
            collectFields(type, fieldName + ".", typeVariables, typesOnPath, fields);
        }
    }

    private static SchemaFieldInfo describeField(String fieldName, Type fieldType, Schema schema) {
        String typeName = Strings.isFilled(schema.type()) ? schema.type() : determineTypeName(fieldType);
        return new SchemaFieldInfo(fieldName,
                                   typeName,
                                   isRequired(schema),
                                   NLS.smartGet(schema.description()),
                                   schema.example());
    }

    private static Type determineCollectionElementType(Type type) {
        if (type instanceof GenericArrayType arrayType) {
            return arrayType.getGenericComponentType();
        }
        Class<?> rawType = determineRawType(type);
        if (rawType != null && rawType.isArray()) {
            return rawType.getComponentType();
        }
        return determineTypeArgument(type, 0);
    }

    private static Type determineMapValueType(Type type) {
        return determineTypeArgument(type, 1);
    }

    private static Type determineTypeArgument(Type type, int index) {
        if (type instanceof ParameterizedType parameterizedType
            && parameterizedType.getActualTypeArguments().length > index) {
            return parameterizedType.getActualTypeArguments()[index];
        }
        return Object.class;
    }

    private static boolean isLeafType(Class<?> type) {
        return type.isPrimitive()
               || type.isEnum()
               || type.isArray() && isLeafType(type.getComponentType())
               || Number.class.isAssignableFrom(type)
               || CharSequence.class.isAssignableFrom(type)
               || Boolean.class.equals(type)
               || Character.class.equals(type)
               || type.getPackageName().startsWith("java.");
    }

    private static Type resolveType(Type type, Map<TypeVariable<?>, Type> typeVariables) {
        if (type instanceof TypeVariable<?> typeVariable) {
            return typeVariables.getOrDefault(typeVariable, Object.class);
        }
        if (type instanceof WildcardType wildcardType) {
            Type[] upperBounds = wildcardType.getUpperBounds();
            return upperBounds.length == 1 ? resolveType(upperBounds[0], typeVariables) : Object.class;
        }
        return type;
    }

    @Nullable
    private static Class<?> determineRawType(Type type) {
        if (type instanceof Class<?> clazz) {
            return clazz;
        }
        if (type instanceof ParameterizedType parameterizedType
            && parameterizedType.getRawType() instanceof Class<?> rawType) {
            return rawType;
        }
        return null;
    }

    private static String determineTypeName(Type type) {
        if (type instanceof GenericArrayType arrayType) {
            return determineTypeName(arrayType.getGenericComponentType()) + "[]";
        }
        if (type instanceof ParameterizedType parameterizedType) {
            String typeArguments = List.of(parameterizedType.getActualTypeArguments())
                                       .stream()
                                       .map(SchemaFieldInfo::determineTypeName)
                                       .collect(java.util.stream.Collectors.joining(", "));
            return determineTypeName(parameterizedType.getRawType()) + "<" + typeArguments + ">";
        }
        if (type instanceof Class<?> clazz) {
            if (clazz.isArray()) {
                return determineTypeName(clazz.getComponentType()) + "[]";
            }
            return clazz.getSimpleName();
        }
        return type.getTypeName();
    }

    @SuppressWarnings("deprecation")
    private static boolean isRequired(Schema schema) {
        return schema.requiredMode() == Schema.RequiredMode.REQUIRED || schema.required();
    }

    public String getName() {
        return name;
    }

    public String getType() {
        return type;
    }

    public boolean isRequired() {
        return required;
    }

    public String getDescription() {
        return description;
    }

    public String getExample() {
        return example;
    }
}
