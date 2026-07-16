/*
 * Made with all the love in the world
 * by scireum in Stuttgart, Germany
 *
 * Copyright by scireum GmbH
 * https://www.scireum.de - info@scireum.de
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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

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
        Type resolvedType = SchemaReflection.resolveType(type, typeVariables);
        Class<?> rawType = SchemaReflection.determineRawType(resolvedType);
        if (rawType == null) {
            return;
        }
        if (rawType.isArray() || Collection.class.isAssignableFrom(rawType)) {
            collectFields(SchemaReflection.determineCollectionElementType(resolvedType),
                          prefix,
                          typeVariables,
                          typesOnPath,
                          fields);
            return;
        }
        if (Map.class.isAssignableFrom(rawType)) {
            collectFields(SchemaReflection.determineMapValueType(resolvedType),
                          prefix,
                          typeVariables,
                          typesOnPath,
                          fields);
            return;
        }
        if (SchemaReflection.isLeafType(rawType)) {
            return;
        }
        if (!typesOnPath.add(rawType)) {
            return;
        }

        Map<TypeVariable<?>, Type> nestedTypeVariables =
                SchemaReflection.resolveTypeVariables(resolvedType, rawType, typeVariables);

        collectFields(rawType.getSuperclass(), prefix, nestedTypeVariables, typesOnPath, fields);
        for (Field field : rawType.getDeclaredFields()) {
            Schema schema = field.getAnnotation(Schema.class);
            if (schema == null) {
                continue;
            }

            Type fieldType = SchemaReflection.resolveType(field.getGenericType(), nestedTypeVariables);
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
        Class<?> rawType = SchemaReflection.determineRawType(type);
        if (rawType == null) {
            return;
        }
        if (rawType.isArray() || Collection.class.isAssignableFrom(rawType)) {
            collectFields(SchemaReflection.determineCollectionElementType(type),
                          fieldName + "[].",
                          typeVariables,
                          typesOnPath,
                          fields);
        } else if (Map.class.isAssignableFrom(rawType)) {
            collectFields(SchemaReflection.determineMapValueType(type),
                          fieldName + "[].",
                          typeVariables,
                          typesOnPath,
                          fields);
        } else if (!SchemaReflection.isLeafType(rawType)) {
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

    private static String determineTypeName(Type type) {
        if (type instanceof GenericArrayType arrayType) {
            return determineTypeName(arrayType.getGenericComponentType()) + "[]";
        }
        if (type instanceof ParameterizedType parameterizedType) {
            String typeArguments = Arrays.stream(parameterizedType.getActualTypeArguments())
                                         .map(SchemaFieldInfo::determineTypeName)
                                         .collect(Collectors.joining(", "));
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
