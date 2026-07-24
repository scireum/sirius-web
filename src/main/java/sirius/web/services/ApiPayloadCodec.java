/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
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
     * Returns the format which is handled by this codec.
     *
     * @return the format which is handled by this codec
     */
    Format getFormat();

    /**
     * Reads the request body and deserializes it into the given input type.
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
