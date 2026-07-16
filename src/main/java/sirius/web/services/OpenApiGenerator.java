/*
 * Made with all the love in the world
 * by scireum in Stuttgart, Germany
 *
 * Copyright by scireum GmbH
 * https://www.scireum.de - info@scireum.de
 */

package sirius.web.services;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.enums.ParameterIn;
import io.swagger.v3.oas.annotations.headers.Header;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Encoding;
import io.swagger.v3.oas.annotations.media.ExampleObject;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.parameters.RequestBody;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import sirius.kernel.commons.Json;
import sirius.kernel.commons.Strings;
import sirius.kernel.nls.NLS;

import javax.annotation.Nullable;
import java.lang.reflect.Field;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;
import java.time.LocalDate;
import java.time.temporal.Temporal;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * Generates an <a href="https://spec.openapis.org/oas/v3.1.0">OpenAPI 3.1</a> document for a {@link PublicApiInfo}.
 * <p>
 * The document is derived from the same metadata which is used to render the API explorer at <tt>/system/api</tt>:
 * the {@link PublicService} / {@link sirius.web.controller.Routed} annotations, the {@code http.api} configuration and
 * the {@link Schema} annotations placed on the request and response POJOs of
 * {@linkplain sirius.web.controller.Route#isMappedPayload() mapped services}.
 * <p>
 * Request and response bodies of mapped services are expanded into fully nested {@code components/schemas} entries,
 * referencing each other via {@code $ref}. Self-referential types are handled gracefully.
 * <p>
 * A generator instance is stateful (it collects the referenced component schemas while building the paths) and must
 * therefore not be reused across multiple documents. Create a fresh instance per document instead.
 */
public class OpenApiGenerator {

    private static final String OPENAPI_VERSION = "3.1.0";
    private static final String DOCUMENT_VERSION = "1.0.0";
    private static final String MEDIA_TYPE_JSON = "application/json";
    private static final String MEDIA_TYPE_XML = "application/xml";

    private static final String FIELD_TYPE = "type";
    private static final String FIELD_FORMAT = "format";
    private static final String FIELD_ITEMS = "items";
    private static final String FIELD_SCHEMA = "schema";
    private static final String FIELD_CONTENT = "content";
    private static final String FIELD_PROPERTIES = "properties";
    private static final String FIELD_REQUIRED = "required";
    private static final String FIELD_DESCRIPTION = "description";
    private static final String FIELD_EXAMPLE = "example";
    private static final String FIELD_ADDITIONAL_PROPERTIES = "additionalProperties";

    private static final String TYPE_OBJECT = "object";
    private static final String TYPE_ARRAY = "array";
    private static final String TYPE_STRING = "string";
    private static final String TYPE_INTEGER = "integer";
    private static final String TYPE_NUMBER = "number";
    private static final String TYPE_BOOLEAN = "boolean";

    private final Map<String, ObjectNode> componentSchemas = new LinkedHashMap<>();
    private final Map<String, Type> typesBySchemaName = new HashMap<>();
    private final Map<Type, String> schemaNamesByType = new HashMap<>();

    /**
     * Generates the OpenAPI document for the given API.
     *
     * @param api the API to describe
     * @return the OpenAPI document as JSON object
     */
    public ObjectNode generate(PublicApiInfo api) {
        ObjectNode document = Json.createObject();
        document.put("openapi", OPENAPI_VERSION);
        document.set("info", buildInfo(api));

        // The paths are built first, as this collects the referenced component schemas as a side effect.
        ObjectNode paths = buildPaths(api);
        document.set("paths", paths);

        if (!componentSchemas.isEmpty()) {
            ObjectNode components = Json.createObject();
            ObjectNode schemas = Json.createObject();
            componentSchemas.forEach(schemas::set);
            components.set("schemas", schemas);
            document.set("components", components);
        }

        return document;
    }

    /**
     * Builds the OpenAPI schema node for the given Java type.
     * <p>
     * Complex POJOs are represented as a {@code $ref} pointing into {@link #getComponentSchemas()}, which is populated
     * as a side effect. This mirrors the schema generation used while building a whole document and is primarily useful
     * to describe a single request or response type in isolation.
     *
     * @param type the type to describe
     * @return the schema node, either an inline schema or a {@code $ref} to a component schema
     */
    public ObjectNode schemaForType(Type type) {
        return schemaFor(type, new HashMap<>());
    }

    /**
     * Returns the component schemas collected while generating a document or resolving individual
     * {@linkplain #schemaForType(Type) types}.
     *
     * @return the collected component schemas keyed by their schema name
     */
    public Map<String, ObjectNode> getComponentSchemas() {
        return Collections.unmodifiableMap(componentSchemas);
    }

    private ObjectNode buildInfo(PublicApiInfo api) {
        ObjectNode info = Json.createObject();
        info.put("title", api.getLabel());
        info.put("version", DOCUMENT_VERSION);
        if (Strings.isFilled(api.getDescription())) {
            info.put(FIELD_DESCRIPTION, api.getDescription());
        }
        return info;
    }

    private ObjectNode buildPaths(PublicApiInfo api) {
        ObjectNode paths = Json.createObject();
        for (PublicApiSectionInfo section : api.getSections()) {
            for (PublicServiceInfo service : section.getServices()) {
                addService(paths, section, service);
            }
        }
        return paths;
    }

    private void addService(ObjectNode paths, PublicApiSectionInfo section, PublicServiceInfo service) {
        String path = determineOpenApiPath(service);
        ObjectNode pathItem;
        if (paths.get(path) instanceof ObjectNode existingPathItem) {
            pathItem = existingPathItem;
        } else {
            pathItem = Json.createObject();
            paths.set(path, pathItem);
        }

        ObjectNode operation = Json.createObject();
        operation.put("operationId", buildOperationId(service));
        if (Strings.isFilled(service.getLabel())) {
            operation.put("summary", service.getLabel());
        }
        if (Strings.isFilled(service.getDescription())) {
            operation.put(FIELD_DESCRIPTION, service.getDescription());
        }
        if (service.isDeprecated()) {
            operation.put("deprecated", true);
        }
        if (Strings.isFilled(section.getLabel())) {
            operation.set("tags", Json.createArray().add(section.getLabel()));
        }

        ArrayNode parameters = buildParameters(service);
        if (!parameters.isEmpty()) {
            operation.set("parameters", parameters);
        }

        ObjectNode requestBody = buildRequestBody(service);
        if (requestBody != null) {
            operation.set("requestBody", requestBody);
        }

        operation.set("responses", buildResponses(service));

        pathItem.set(service.getHttpMethod().name().toLowerCase(), operation);
    }

    private String buildOperationId(PublicServiceInfo service) {
        return service.getHttpMethod().name().toLowerCase()
               + determineOpenApiPath(service).replaceAll("[^a-zA-Z0-9]+", "-");
    }

    private ArrayNode buildParameters(PublicServiceInfo service) {
        ArrayNode parameters = Json.createArray();
        service.getPathComponents().forEach(parameter -> parameters.add(mapParameter(parameter, true)));
        collectImplicitPathParameters(service, parameters);
        service.getParameters().forEach(parameter -> parameters.add(mapParameter(parameter, false)));
        return parameters;
    }

    private String determineOpenApiPath(PublicServiceInfo service) {
        java.util.regex.Matcher matcher = java.util.regex.Pattern.compile(":([0-9]+)").matcher(service.getUri());
        StringBuffer path = new StringBuffer();
        while (matcher.find()) {
            String name = pathParameterName(service, matcher.group(1));
            matcher.appendReplacement(path, "{" + name + "}");
        }
        matcher.appendTail(path);
        return path.toString();
    }

    private String pathParameterName(PublicServiceInfo service, String index) {
        int parameterIndex = Integer.parseInt(index) - 1;
        return parameterIndex < service.getPathComponents().size()
               ? service.getPathComponents().get(parameterIndex).name()
               : "parameter" + index;
    }

    private void collectImplicitPathParameters(PublicServiceInfo service, ArrayNode parameters) {
        java.util.regex.Matcher matcher = java.util.regex.Pattern.compile(":([0-9]+)").matcher(service.getUri());
        while (matcher.find()) {
            String name = pathParameterName(service, matcher.group(1));
            boolean alreadyDocumented = service.getPathComponents()
                                               .stream()
                                               .anyMatch(parameter -> name.equals(parameter.name()));
            if (!alreadyDocumented) {
                ObjectNode parameter = Json.createObject();
                parameter.put("name", name);
                parameter.put("in", "path");
                parameter.put(FIELD_REQUIRED, true);
                ObjectNode schema = Json.createObject();
                schema.put(FIELD_TYPE, TYPE_STRING);
                parameter.set(FIELD_SCHEMA, schema);
                parameters.add(parameter);
            }
        }
    }

    private ObjectNode mapParameter(Parameter parameter, boolean forcePath) {
        ObjectNode node = Json.createObject();
        node.put("name", parameter.name());
        node.put("in", forcePath ? "path" : mapParameterLocation(parameter.in()));
        node.put(FIELD_REQUIRED, forcePath || parameter.required());
        if (Strings.isFilled(parameter.description())) {
            node.put(FIELD_DESCRIPTION, NLS.smartGet(parameter.description()));
        }

        ObjectNode schema = Json.createObject();
        Schema parameterSchema = parameter.schema();
        schema.put(FIELD_TYPE, Strings.isFilled(parameterSchema.type()) ? parameterSchema.type() : TYPE_STRING);
        if (Strings.isFilled(parameterSchema.format())) {
            schema.put(FIELD_FORMAT, parameterSchema.format());
        }
        node.set(FIELD_SCHEMA, schema);

        if (Strings.isFilled(parameter.example())) {
            node.set(FIELD_EXAMPLE, typedExample(schema.path(FIELD_TYPE).asText(), parameter.example()));
        }
        return node;
    }

    private String mapParameterLocation(ParameterIn location) {
        return switch (location) {
            case PATH -> "path";
            case HEADER -> "header";
            case COOKIE -> "cookie";
            default -> "query";
        };
    }

    @Nullable
    private ObjectNode buildRequestBody(PublicServiceInfo service) {
        if (!service.getRequestBodies().isEmpty()) {
            return mapAnnotatedRequestBody(service.getRequestBodies().getFirst());
        }
        if (service.getInputType() == null) {
            return null;
        }

        ObjectNode requestBody = Json.createObject();
        requestBody.put(FIELD_REQUIRED, true);
        ObjectNode content = Json.createObject();
        ObjectNode mediaType = Json.createObject();
        mediaType.set(FIELD_SCHEMA, schemaFor(service.getInputType(), new HashMap<>()));
        content.set(mediaTypeFor(service), mediaType);
        requestBody.set(FIELD_CONTENT, content);
        return requestBody;
    }

    private ObjectNode mapAnnotatedRequestBody(RequestBody annotation) {
        ObjectNode requestBody = Json.createObject();
        if (Strings.isFilled(annotation.description())) {
            requestBody.put(FIELD_DESCRIPTION, NLS.smartGet(annotation.description()));
        }
        requestBody.put(FIELD_REQUIRED, annotation.required());
        ObjectNode content = mapAnnotatedContent(annotation.content());
        if (content != null) {
            requestBody.set(FIELD_CONTENT, content);
        }
        return requestBody;
    }

    private ObjectNode buildResponses(PublicServiceInfo service) {
        ObjectNode responses = Json.createObject();
        if (!service.getResponses().isEmpty()) {
            service.getResponses()
                   .forEach(response -> responses.set(response.responseCode(), mapAnnotatedResponse(response)));
            return responses;
        }

        ObjectNode okResponse = Json.createObject();
        okResponse.put(FIELD_DESCRIPTION, "Successful response");
        if (service.getOutputType() != null) {
            ObjectNode content = Json.createObject();
            ObjectNode mediaType = Json.createObject();
            mediaType.set(FIELD_SCHEMA, schemaFor(service.getOutputType(), new HashMap<>()));
            content.set(mediaTypeFor(service), mediaType);
            okResponse.set(FIELD_CONTENT, content);
        }
        responses.set("200", okResponse);
        return responses;
    }

    private ObjectNode mapAnnotatedResponse(ApiResponse response) {
        ObjectNode node = Json.createObject();
        node.put(FIELD_DESCRIPTION,
                 Strings.isFilled(response.description()) ? NLS.smartGet(response.description()) : "");
        ObjectNode content = mapAnnotatedContent(response.content());
        if (content != null) {
            node.set(FIELD_CONTENT, content);
        }
        return node;
    }

    @Nullable
    private ObjectNode mapAnnotatedContent(Content[] contents) {
        ObjectNode content = Json.createObject();
        for (Content entry : contents) {
            if (Strings.isFilled(entry.mediaType())) {
                ObjectNode mediaType = Json.createObject();
                ObjectNode schema = mapAnnotatedSchema(entry.schema());
                if (schema != null) {
                    mediaType.set(FIELD_SCHEMA, schema);
                }
                ObjectNode examples = mapAnnotatedExamples(entry.examples());
                if (examples != null) {
                    mediaType.set("examples", examples);
                }
                ObjectNode encoding = mapAnnotatedEncoding(entry.encoding());
                if (encoding != null) {
                    mediaType.set("encoding", encoding);
                }
                content.set(entry.mediaType(), mediaType);
            }
        }
        return content.isEmpty() ? null : content;
    }

    @Nullable
    private ObjectNode mapAnnotatedEncoding(Encoding[] encodings) {
        if (encodings.length == 0) {
            return null;
        }

        ObjectNode result = Json.createObject();
        for (Encoding encoding : encodings) {
            if (Strings.isEmpty(encoding.name())) {
                continue;
            }
            ObjectNode value = Json.createObject();
            if (Strings.isFilled(encoding.contentType())) {
                value.put("contentType", encoding.contentType());
            }
            if (Strings.isFilled(encoding.style())) {
                value.put("style", encoding.style());
            }
            if (encoding.explode()) {
                value.put("explode", true);
            }
            if (encoding.allowReserved()) {
                value.put("allowReserved", true);
            }
            ObjectNode headers = mapEncodingHeaders(encoding.headers());
            if (headers != null) {
                value.set("headers", headers);
            }
            result.set(encoding.name(), value);
        }
        return result.isEmpty() ? null : result;
    }

    @Nullable
    private ObjectNode mapEncodingHeaders(Header[] headers) {
        if (headers.length == 0) {
            return null;
        }

        ObjectNode result = Json.createObject();
        for (Header header : headers) {
            if (Strings.isEmpty(header.name())) {
                continue;
            }
            ObjectNode value = Json.createObject();
            if (Strings.isFilled(header.description())) {
                value.put(FIELD_DESCRIPTION, NLS.smartGet(header.description()));
            }
            ObjectNode schema = mapAnnotatedSchema(header.schema());
            if (schema != null) {
                value.set(FIELD_SCHEMA, schema);
            }
            result.set(header.name(), value);
        }
        return result.isEmpty() ? null : result;
    }

    @Nullable
    private ObjectNode mapAnnotatedSchema(Schema annotation) {
        if (Strings.isFilled(annotation.ref())) {
            ObjectNode schema = Json.createObject();
            schema.put("$ref", annotation.ref());
            return schema;
        }
        if (annotation.implementation() == Void.class && Strings.isEmpty(annotation.type())) {
            return null;
        }

        ObjectNode schema = annotation.implementation() == Void.class
                            ? Json.createObject()
                            : schemaFor(annotation.implementation(), new HashMap<>());
        if (Strings.isFilled(annotation.type()) && !schema.has("$ref")) {
            schema.put(FIELD_TYPE, annotation.type());
        }
        if (Strings.isFilled(annotation.format()) && !schema.has("$ref")) {
            schema.put(FIELD_FORMAT, annotation.format());
        }
        if (Strings.isFilled(annotation.description())) {
            schema.put(FIELD_DESCRIPTION, NLS.smartGet(annotation.description()));
        }
        if (Strings.isFilled(annotation.example())) {
            putExample(schema, annotation.example());
        }
        return schema;
    }

    @Nullable
    private ObjectNode mapAnnotatedExamples(ExampleObject[] exampleObjects) {
        if (exampleObjects.length == 0) {
            return null;
        }

        ObjectNode examples = Json.createObject();
        for (int index = 0; index < exampleObjects.length; index++) {
            ExampleObject example = exampleObjects[index];
            String name = Strings.isFilled(example.name()) ? example.name() : "example" + (index + 1);
            ObjectNode exampleNode = Json.createObject();
            if (Strings.isFilled(example.summary())) {
                exampleNode.put("summary", example.summary());
            }
            if (Strings.isFilled(example.description())) {
                exampleNode.put(FIELD_DESCRIPTION, example.description());
            }
            if (Strings.isFilled(example.externalValue())) {
                exampleNode.put("externalValue", example.externalValue());
            } else if (Strings.isFilled(example.value())) {
                exampleNode.set("value", parseExampleValue(example.value()));
            }
            examples.set(name, exampleNode);
        }
        return examples;
    }

    private JsonNode parseExampleValue(String value) {
        try {
            return Json.MAPPER.readTree(value);
        } catch (Exception ignored) {
            return Json.MAPPER.getNodeFactory().textNode(value);
        }
    }

    private String mediaTypeFor(PublicServiceInfo service) {
        return service.determineEffectiveFormat() == Format.XML ? MEDIA_TYPE_XML : MEDIA_TYPE_JSON;
    }

    /**
     * Builds the OpenAPI schema for the given type.
     * <p>
     * Leaf types (primitives, strings, enums, temporal types, ...) are described inline. Collections and maps are
     * described as {@code array} respectively {@code object} with typed items. Complex POJOs are registered as a named
     * {@code components/schemas} entry and referenced via {@code $ref}.
     */
    private ObjectNode schemaFor(Type type, Map<TypeVariable<?>, Type> typeVariables) {
        Type resolvedType = SchemaReflection.resolveType(type, typeVariables);
        Class<?> rawType = SchemaReflection.determineRawType(resolvedType);
        if (rawType == null) {
            return objectSchema();
        }

        if (rawType.isArray() || Collection.class.isAssignableFrom(rawType)) {
            ObjectNode schema = Json.createObject();
            schema.put(FIELD_TYPE, TYPE_ARRAY);
            schema.set(FIELD_ITEMS,
                       schemaFor(SchemaReflection.determineCollectionElementType(resolvedType), typeVariables));
            return schema;
        }
        if (Map.class.isAssignableFrom(rawType)) {
            ObjectNode schema = Json.createObject();
            schema.put(FIELD_TYPE, TYPE_OBJECT);
            schema.set(FIELD_ADDITIONAL_PROPERTIES,
                       schemaFor(SchemaReflection.determineMapValueType(resolvedType), typeVariables));
            return schema;
        }
        if (SchemaReflection.isLeafType(rawType)) {
            return leafSchema(rawType);
        }

        String schemaName = registerComponentSchema(resolvedType, rawType, typeVariables);
        ObjectNode reference = Json.createObject();
        reference.put("$ref", "#/components/schemas/" + schemaName);
        return reference;
    }

    private String registerComponentSchema(Type resolvedType,
                                           Class<?> rawType,
                                           Map<TypeVariable<?>, Type> typeVariables) {
        String schemaName = determineSchemaName(resolvedType, rawType);
        if (componentSchemas.containsKey(schemaName)) {
            return schemaName;
        }

        // Reserve the entry before recursing into the fields, so self-referential types terminate.
        ObjectNode schema = Json.createObject();
        componentSchemas.put(schemaName, schema);
        typesBySchemaName.put(schemaName, resolvedType);
        schemaNamesByType.put(resolvedType, schemaName);

        schema.put(FIELD_TYPE, TYPE_OBJECT);
        ObjectNode properties = Json.createObject();
        ArrayNode required = Json.createArray();
        collectProperties(resolvedType, typeVariables, properties, required);
        if (!properties.isEmpty()) {
            schema.set(FIELD_PROPERTIES, properties);
        }
        if (!required.isEmpty()) {
            schema.set(FIELD_REQUIRED, required);
        }
        return schemaName;
    }

    private String determineSchemaName(Type resolvedType, Class<?> rawType) {
        String existingName = schemaNamesByType.get(resolvedType);
        if (existingName != null) {
            return existingName;
        }

        String simpleName = rawType.getSimpleName();
        if (!(resolvedType instanceof ParameterizedType) && !typesBySchemaName.containsKey(simpleName)) {
            return simpleName;
        }
        return resolvedType.getTypeName().replaceAll("[^A-Za-z0-9]+", "_");
    }

    private void collectProperties(Type type,
                                   Map<TypeVariable<?>, Type> typeVariables,
                                   ObjectNode properties,
                                   ArrayNode required) {
        Class<?> rawType = SchemaReflection.determineRawType(type);
        if (rawType == null || SchemaReflection.isLeafType(rawType)) {
            return;
        }

        Map<TypeVariable<?>, Type> nestedTypeVariables =
                SchemaReflection.resolveTypeVariables(type, rawType, typeVariables);

        collectProperties(rawType.getGenericSuperclass(), nestedTypeVariables, properties, required);

        for (Field field : rawType.getDeclaredFields()) {
            Schema fieldSchema = field.getAnnotation(Schema.class);
            if (fieldSchema == null) {
                continue;
            }
            String fieldName = Strings.isFilled(fieldSchema.name()) ? fieldSchema.name() : field.getName();
            Type fieldType = SchemaReflection.resolveType(field.getGenericType(), nestedTypeVariables);
            ObjectNode propertySchema = schemaFor(fieldType, nestedTypeVariables);
            applyFieldAnnotation(propertySchema, fieldSchema);
            properties.set(fieldName, propertySchema);
            if (isRequired(fieldSchema)) {
                required.add(fieldName);
            }
        }
    }

    private void applyFieldAnnotation(ObjectNode propertySchema, Schema fieldSchema) {
        // OpenAPI 3.1 allows annotation keywords next to a $ref, so descriptions on referenced schemas are kept.
        if (Strings.isFilled(fieldSchema.type()) && !propertySchema.has("$ref")) {
            propertySchema.put(FIELD_TYPE, fieldSchema.type());
        }
        if (Strings.isFilled(fieldSchema.format()) && !propertySchema.has("$ref")) {
            propertySchema.put(FIELD_FORMAT, fieldSchema.format());
        }
        if (Strings.isFilled(fieldSchema.description())) {
            propertySchema.put(FIELD_DESCRIPTION, NLS.smartGet(fieldSchema.description()));
        }
        if (Strings.isFilled(fieldSchema.example())) {
            putExample(propertySchema, fieldSchema.example());
        }
    }

    private void putExample(ObjectNode schema, String example) {
        schema.set(FIELD_EXAMPLE, typedExample(schema.path(FIELD_TYPE).asText(), example));
    }

    private JsonNode typedExample(String type, String example) {
        return switch (type) {
            case TYPE_INTEGER -> {
                try {
                    yield Json.MAPPER.getNodeFactory().numberNode(Long.parseLong(example));
                } catch (NumberFormatException ignored) {
                    yield Json.MAPPER.getNodeFactory().textNode(example);
                }
            }
            case TYPE_NUMBER -> {
                try {
                    yield Json.MAPPER.getNodeFactory().numberNode(Double.parseDouble(example));
                } catch (NumberFormatException ignored) {
                    yield Json.MAPPER.getNodeFactory().textNode(example);
                }
            }
            case TYPE_BOOLEAN -> Json.MAPPER.getNodeFactory().booleanNode(Boolean.parseBoolean(example));
            default -> Json.MAPPER.getNodeFactory().textNode(example);
        };
    }

    private ObjectNode leafSchema(Class<?> rawType) {
        ObjectNode schema = Json.createObject();
        if (rawType.isEnum()) {
            schema.put(FIELD_TYPE, TYPE_STRING);
            ArrayNode enumValues = Json.createArray();
            for (Object constant : rawType.getEnumConstants()) {
                enumValues.add(((Enum<?>) constant).name());
            }
            schema.set("enum", enumValues);
            return schema;
        }
        if (rawType == boolean.class || rawType == Boolean.class) {
            schema.put(FIELD_TYPE, TYPE_BOOLEAN);
            return schema;
        }
        if (isIntegerType(rawType)) {
            schema.put(FIELD_TYPE, TYPE_INTEGER);
            if (rawType == long.class || rawType == Long.class) {
                schema.put(FIELD_FORMAT, "int64");
            } else if (rawType == int.class || rawType == Integer.class) {
                schema.put(FIELD_FORMAT, "int32");
            }
            return schema;
        }
        if (Number.class.isAssignableFrom(rawType) || rawType.isPrimitive() && rawType != char.class) {
            schema.put(FIELD_TYPE, TYPE_NUMBER);
            return schema;
        }
        if (Temporal.class.isAssignableFrom(rawType)) {
            schema.put(FIELD_TYPE, TYPE_STRING);
            schema.put(FIELD_FORMAT, rawType == LocalDate.class ? "date" : "date-time");
            return schema;
        }
        schema.put(FIELD_TYPE, TYPE_STRING);
        return schema;
    }

    private boolean isIntegerType(Class<?> rawType) {
        return rawType == int.class
               || rawType == Integer.class
               || rawType == long.class
               || rawType == Long.class
               || rawType == short.class
               || rawType == Short.class
               || rawType == byte.class
               || rawType == Byte.class
               || java.math.BigInteger.class.isAssignableFrom(rawType);
    }

    private ObjectNode objectSchema() {
        ObjectNode schema = Json.createObject();
        schema.put(FIELD_TYPE, TYPE_OBJECT);
        return schema;
    }

    @SuppressWarnings("deprecation")
    private boolean isRequired(Schema schema) {
        return schema.requiredMode() == Schema.RequiredMode.REQUIRED || schema.required();
    }
}
