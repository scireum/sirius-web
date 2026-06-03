/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.services;

import io.netty.handler.codec.http.HttpResponseStatus;
import sirius.kernel.commons.Json;
import sirius.kernel.di.std.Register;
import sirius.kernel.health.Exceptions;
import sirius.web.http.MimeHelper;
import sirius.web.http.WebContext;
import sirius.web.http.WebServer;

import javax.annotation.Nullable;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;

/**
 * Provides the {@link Format#JSON JSON} implementation of an {@link ApiPayloadCodec}.
 * <p>
 * The request body is parsed via the central {@link Json#MAPPER Jackson mapper} and the result is serialized directly
 * into the JSON response body.
 */
@Register
public class JsonApiPayloadCodec implements ApiPayloadCodec {

    @Override
    public Format getFormat() {
        return Format.JSON;
    }

    @Override
    public Object readBody(WebContext webContext, Class<?> inputType) {
        try {
            return Json.MAPPER.treeToValue(webContext.getJSONContent(), inputType);
        } catch (Exception exception) {
            throw Exceptions.createHandled()
                            .to(WebServer.LOG)
                            .error(exception)
                            .withSystemErrorMessage("Cannot parse the request body as '%s': %s (%s)",
                                                    inputType.getName())
                            .handle();
        }
    }

    @Override
    public void writeResult(WebContext webContext, @Nullable Object result) {
        try (OutputStream out = webContext.respondWith()
                                          .outputStream(HttpResponseStatus.OK,
                                                        MimeHelper.APPLICATION_JSON + ";charset=" + StandardCharsets.UTF_8.name())) {
            out.write(Json.MAPPER.writeValueAsBytes(result));
        } catch (Exception exception) {
            throw Exceptions.handle()
                            .to(WebServer.LOG)
                            .error(exception)
                            .withSystemErrorMessage("Cannot serialize the result as JSON: %s (%s)")
                            .handle();
        }
    }
}
