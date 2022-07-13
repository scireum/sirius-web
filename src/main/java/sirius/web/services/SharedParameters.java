/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.services;

import io.swagger.v3.oas.annotations.Parameter;
import sirius.kernel.di.std.AutoRegister;

import java.util.List;

/**
 * Defines a set of shared parameters for public services.
 * <p>
 * Use {@link ParametersFrom} annotations to copy parameters from an instance of this interface to actual public service routes.
 * <p>
 * For easier implementation, consider using the {@link SharedParametersAdapter}.
 */
@AutoRegister
public interface SharedParameters {

    /**
     * Returns the list of shared {@link Parameter} descriptions.
     *
     * @return an unmodifiable list of parameter descriptions
     */
    List<Parameter> getParameters();
}
