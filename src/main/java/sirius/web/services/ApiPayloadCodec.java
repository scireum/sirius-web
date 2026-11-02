/*
 * Made with all the love in the world
 * by scireum in Stuttgart, Germany
 *
 * Copyright by scireum GmbH
 * https://www.scireum.de - info@scireum.de
 */

package sirius.web.services;

import sirius.kernel.di.std.AutoRegister;
import sirius.web.http.WebContext;

import javax.annotation.Nullable;

/**
 * Handles the (de-)serialization of request and response payloads for {@linkplain sirius.web.controller.Route mapped}
 * service calls.
 * <p>
 * A mapped service method receives its request body deserialized into an input POJO and returns a result object which
 * is serialized directly (without any {@code success}/{@code error} envelope). Each codec is responsible for exactly
 * one {@link Format} and is selected by the {@link sirius.web.controller.ControllerDispatcher} based on the format
 * declared by the route.
 *
 * @see JsonApiPayloadCodec
 */
@AutoRegister
public interface ApiPayloadCodec {

    /**
     * Contains the {@link sirius.web.controller.Controller#ERROR_CODE error code} reported if the request body cannot
     * be read or does not match the input type of the route.
     */
    String ERROR_CODE_INVALID_REQUEST = "invalid_request";

    /**
     * Returns the format which is handled by this codec.
     *
     * @return the format which is handled by this codec
     */
    Format getFormat();

    /**
     * Reads the request body and deserializes it into the given input type.
     * <p>
     * A body which cannot be read or does not match the input type is reported as client error, i.e. with
     * HTTP status 400 and the error code {@link #ERROR_CODE_INVALID_REQUEST}.
     *
     * @param webContext the request providing the body to read
     * @param inputType  the type into which the body is deserialized
     * @return the deserialized input object
     */
    Object readBody(WebContext webContext, Class<?> inputType);

    /**
     * Serializes the given result object and sends it as response body.
     * <p>
     * The result is written as-is, i.e. without any wrapping envelope.
     *
     * @param webContext the request to respond to
     * @param result     the result object to serialize, may be <tt>null</tt>
     */
    void writeResult(WebContext webContext, @Nullable Object result);
}
