/*
 * Made with all the love in the world
 * by scireum in Stuttgart, Germany
 *
 * Copyright by scireum GmbH
 * https://www.scireum.de - info@scireum.de
 */

package sirius.web.services;

import io.netty.handler.codec.http.HttpResponseStatus;
import sirius.kernel.commons.Json;
import sirius.kernel.commons.Strings;
import sirius.kernel.di.std.Register;
import sirius.kernel.health.Exceptions;
import sirius.kernel.health.HandledException;
import sirius.web.controller.Controller;
import sirius.web.http.MimeHelper;
import sirius.web.http.WebContext;
import sirius.web.http.WebServer;
import tools.jackson.core.JacksonException;
import tools.jackson.databind.exc.UnrecognizedPropertyException;
import tools.jackson.databind.node.ObjectNode;

import javax.annotation.Nullable;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;

/**
 * Provides the {@link Format#JSON JSON} implementation of an {@link ApiPayloadCodec}.
 * <p>
 * The request body is parsed via the central {@link Json#MAPPER Jackson mapper} and the result is serialized directly
 * into the JSON response body.
 * <p>
 * A body which cannot be parsed or does not match the input type is rejected as client error (see
 * {@link #ERROR_CODE_INVALID_REQUEST}). The reported message names the affected field (also reported via
 * {@link Controller#ERROR_FIELD}), but never the Java types involved.
 */
@Register
public class JsonApiPayloadCodec implements ApiPayloadCodec {

    @Override
    public Format getFormat() {
        return Format.JSON;
    }

    @Override
    public Object readBody(WebContext webContext, Class<?> inputType) {
        ObjectNode body;
        try {
            body = webContext.getJSONContent();
        } catch (HandledException exception) {
            throw invalidBody(exception);
        }

        try {
            return Json.MAPPER.treeToValue(body, inputType);
        } catch (UnrecognizedPropertyException exception) {
            throw invalidField(exception, "JsonApiPayloadCodec.unknownField");
        } catch (JacksonException exception) {
            throw invalidField(exception, "JsonApiPayloadCodec.invalidField");
        } catch (Exception exception) {
            throw invalidBody(exception);
        }
    }

    /**
     * Reports a body which cannot be read at all.
     * <p>
     * Errors raised without a cause (e.g. a missing or too large body) keep their message, while the message of
     * parser errors is replaced, as it reveals internals of the parser and the server.
     */
    private HandledException invalidBody(Exception exception) {
        if (exception instanceof HandledException handledException && handledException.getCause() == null) {
            return handledException.withHint(Controller.HTTP_STATUS, HttpResponseStatus.BAD_REQUEST.code())
                                   .withHint(Controller.ERROR_CODE, ERROR_CODE_INVALID_REQUEST);
        }

        WebServer.LOG.FINE("Cannot read the request body: %s", exception.getMessage());
        return Exceptions.createHandled()
                         .withNLSKey("JsonApiPayloadCodec.invalidBody")
                         .hint(Controller.HTTP_STATUS, HttpResponseStatus.BAD_REQUEST.code())
                         .hint(Controller.ERROR_CODE, ERROR_CODE_INVALID_REQUEST)
                         .handle();
    }

    /**
     * Reports a body which is valid JSON but does not match the input type, naming the affected field instead of
     * the Java types involved.
     */
    private HandledException invalidField(JacksonException exception, String nlsKey) {
        String field = determineFieldPath(exception);
        if (Strings.isEmpty(field)) {
            return invalidBody(exception);
        }

        WebServer.LOG.FINE("Cannot read the request body: %s", exception.getMessage());
        return Exceptions.createHandled()
                         .withNLSKey(nlsKey)
                         .set("field", field)
                         .hint(Controller.HTTP_STATUS, HttpResponseStatus.BAD_REQUEST.code())
                         .hint(Controller.ERROR_CODE, ERROR_CODE_INVALID_REQUEST)
                         .hint(Controller.ERROR_FIELD, field)
                         .handle();
    }

    /**
     * Renders the path of the given exception like <tt>positions[0].quantity</tt>.
     */
    private String determineFieldPath(JacksonException exception) {
        StringBuilder path = new StringBuilder();
        for (JacksonException.Reference reference : exception.getPath()) {
            if (reference.getPropertyName() != null) {
                if (!path.isEmpty()) {
                    path.append('.');
                }
                path.append(reference.getPropertyName());
            } else if (reference.getIndex() >= 0) {
                path.append('[').append(reference.getIndex()).append(']');
            }
        }
        return path.toString();
    }

    @Override
    public void writeResult(WebContext webContext, @Nullable Object result) {
        // Serialize before opening the response stream - once the stream is opened, the 200 OK status is
        // committed and a serialization failure could no longer be reported as a proper error response.
        byte[] json;
        try {
            json = Json.MAPPER.writeValueAsBytes(result);
        } catch (Exception exception) {
            throw Exceptions.handle()
                            .to(WebServer.LOG)
                            .error(exception)
                            .withSystemErrorMessage("Cannot serialize the result as JSON: %s (%s)")
                            .handle();
        }

        try (OutputStream out = webContext.respondWith()
                                          .outputStream(HttpResponseStatus.OK,
                                                        MimeHelper.APPLICATION_JSON
                                                        + ";charset="
                                                        + StandardCharsets.UTF_8.name())) {
            out.write(json);
        } catch (Exception exception) {
            throw Exceptions.handle()
                            .to(WebServer.LOG)
                            .error(exception)
                            .withSystemErrorMessage("Cannot write the JSON response: %s (%s)")
                            .handle();
        }
    }
}
