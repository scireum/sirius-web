/*
 * Made with all the love in the world
 * by scireum in Stuttgart, Germany
 *
 * Copyright by scireum GmbH
 * https://www.scireum.de - info@scireum.de
 */

package sirius.web.services;

import io.netty.handler.codec.http.HttpResponseStatus;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import sirius.kernel.di.std.Register;
import sirius.web.controller.BasicController;
import sirius.web.controller.GreetInput;
import sirius.web.controller.GreetResult;
import sirius.web.controller.Routed;
import sirius.web.http.WebContext;

/**
 * Provides public services whose documented responses are verified by the OpenAPI generation tests.
 */
@Register
public class OpenApiTestController extends BasicController {

    @Routed(value = "/test/openapi/error-responses", skipCsrfValidation = true)
    @PublicService(apiName = "openapi-test", priority = 1)
    @ApiResponsesFrom(DefaultErrorResponsesJson.class)
    @ApiResponse(responseCode = "403",
            description = "The greeting is not permitted",
            content = @Content(mediaType = "application/json"))
    public GreetResult greetWithErrorResponses(WebContext webContext, GreetInput input) {
        return new GreetResult("Hello " + input.getName());
    }

    @Routed("/test/openapi/success-response")
    @PublicService(apiName = "openapi-test", priority = 2, format = Format.RAW)
    @ApiResponsesFrom(DefaultErrorResponsesJson.class)
    @ApiResponse(responseCode = "200",
            description = "The greeting as plain text",
            content = @Content(mediaType = "text/plain"))
    public void greetWithSuccessResponse(WebContext webContext) {
        webContext.respondWith().direct(HttpResponseStatus.OK, "Hello");
    }

    @Routed("/test/openapi/legacy-error-responses")
    @PublicService(apiName = "openapi-test", priority = 3, format = Format.RAW)
    @ApiResponsesFrom(DefaultErrorResponsesJson.class)
    public void greetWithLegacyErrorResponses(WebContext webContext) {
        webContext.respondWith().direct(HttpResponseStatus.OK, "Hello");
    }

    @Routed(value = "/test/openapi/range-responses", skipCsrfValidation = true)
    @PublicService(apiName = "openapi-test", priority = 4)
    @ApiResponse(responseCode = "5XX", description = "A server error")
    @ApiResponse(responseCode = "4XX", description = "A client error")
    @ApiResponse(responseCode = "404", description = "The name is unknown")
    public GreetResult greetWithRangeResponses(WebContext webContext, GreetInput input) {
        return new GreetResult("Hello " + input.getName());
    }

    @Routed("/test/openapi/default-response")
    @PublicService(apiName = "openapi-test", priority = 5, format = Format.RAW)
    @ApiResponse(responseCode = "default",
            description = "The greeting as plain text",
            content = @Content(mediaType = "text/plain"))
    @ApiResponse(responseCode = "400", description = "The name is missing")
    public void greetWithDefaultResponse(WebContext webContext) {
        webContext.respondWith().direct(HttpResponseStatus.OK, "Hello");
    }
}
