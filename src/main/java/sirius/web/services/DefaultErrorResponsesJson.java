/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.services;

import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.ExampleObject;
import io.swagger.v3.oas.annotations.responses.ApiResponse;

/**
 * Provides a set of default return codes for JSON APIs.
 */
public class DefaultErrorResponsesJson implements SharedResponses {
    @Override
    @ApiResponse(responseCode = "400",
            description = "Missing parameter or malformed request",
            content = @Content(mediaType = "application/json",examples = @ExampleObject(//language=JSON
            """
                    {
                        "success": false,
                        "error": true,
                        "message": "The parameter {missingParameter} must be filled."
                    }
                    """)))
    @ApiResponse(responseCode = "401",
            description = "Authentication required but none provided",
            content = @Content(mediaType = "application/json"))
    @ApiResponse(responseCode = "403",
            description = "Invalid authentication or missing permission",
            content = @Content(mediaType = "application/json"))
    @ApiResponse(responseCode = "404",
            description = "Resource not found",
            content = @Content(mediaType = "application/json"))
    @ApiResponse(responseCode = "405",
            description = "Incorrect request method, e.g. GET instead of POST",
            content = @Content(mediaType = "application/json"))
    @ApiResponse(responseCode = "500",
            description = "Unexpected server-side error",
            content = @Content(mediaType = "application/json"))
    public void responseMethod() {
        // nothing to do, just a dummy to copy annotations from
    }
}
