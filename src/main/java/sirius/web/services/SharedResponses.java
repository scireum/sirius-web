/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.services;

import io.swagger.v3.oas.annotations.responses.ApiResponse;

/**
 * Defines a set of shared responses for public services.
 * <p>
 * Implementing classes should overwrite the {@linkplain #responseMethod() dummy method} with an empty
 * body and add the shared {@link ApiResponse} annotations there.
 * <p>
 * Use {@link ApiResponsesFrom} annotations to copy responses from an instance of this interface to actual public service
 * routes.
 */
public interface SharedResponses {

    /**
     * Dummy method that is only used for annotating {@link ApiResponse} descriptions. Overwrite it with an empty body
     * and add the shared {@link ApiResponse} annotations.
     */
    void responseMethod();
}
