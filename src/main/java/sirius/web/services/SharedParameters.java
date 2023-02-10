/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.services;

import io.swagger.v3.oas.annotations.Parameter;

/**
 * Defines a set of shared parameters for public services.
 * <p>
 * Implementing classes should overwrite the {@linkplain #parameterMethod() dummy method} with an empty
 * body and add the shared {@link Parameter} annotations there.
 * <p>
 * Use {@link ParametersFrom} annotations to copy parameters from an instance of this interface to actual public service
 * routes.
 */
public interface SharedParameters {

    /**
     * Dummy method that is only used for annotating {@link Parameter} descriptions. Overwrite it with an empty body
     * and add the shared {@link Parameter} annotations.
     */
    void parameterMethod();
}
