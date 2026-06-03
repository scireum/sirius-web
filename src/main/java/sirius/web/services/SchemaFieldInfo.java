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
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.List;

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
     * Introspects the given type and derives a field description for each declared field which carries a
     * {@link Schema} annotation.
     *
     * @param type the type to introspect, may be <tt>null</tt>
     * @return the list of described fields, possibly empty
     */
    public static List<SchemaFieldInfo> forType(@Nullable Type type) {
        if (!(type instanceof Class<?> rawType)) {
            return List.of();
        }

        List<SchemaFieldInfo> fields = new ArrayList<>();
        for (Field field : rawType.getDeclaredFields()) {
            Schema schema = field.getAnnotation(Schema.class);
            if (schema == null) {
                continue;
            }
            fields.add(describeField(field, schema));
        }
        return fields;
    }

    private static SchemaFieldInfo describeField(Field field, Schema schema) {
        String fieldName = Strings.isFilled(schema.name()) ? schema.name() : field.getName();
        String fieldType = Strings.isFilled(schema.type()) ? schema.type() : field.getType().getSimpleName();
        return new SchemaFieldInfo(fieldName,
                                   fieldType,
                                   isRequired(schema),
                                   NLS.smartGet(schema.description()),
                                   schema.example());
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
