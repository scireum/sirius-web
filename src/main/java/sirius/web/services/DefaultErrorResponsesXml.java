/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.services;

import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.responses.ApiResponse;

/**
 * Provides a set of default return codes for XML APIs.
 */
public class DefaultErrorResponsesXml implements SharedResponses {
    @Override
    @ApiResponse(responseCode = "400",
            description = "Missing parameter or malformed request",
            content = @Content(mediaType = "text/xml"))
    @ApiResponse(responseCode = "401",
            description = "Authentication required but none provided",
            content = @Content(mediaType = "text/xml"))
    @ApiResponse(responseCode = "403",
            description = "Invalid authentication or missing permission",
            content = @Content(mediaType = "text/xml"))
    @ApiResponse(responseCode = "404", description = "Resource not found", content = @Content(mediaType = "text/xml"))
    @ApiResponse(responseCode = "405",
            description = "Incorrect request method, e.g. GET instead of POST",
            content = @Content(mediaType = "text/xml"))
    @ApiResponse(responseCode = "500",
            description = "Unexpected server-side error",
            content = @Content(mediaType = "text/xml"))
    public void responseMethod() {
        // nothing to do, just a dummy to copy annotations from
    }
}
