/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.services;

import io.swagger.v3.oas.annotations.Parameter;
import sirius.kernel.health.Exceptions;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

/**
 * Simplifies implementing {@link SharedParameters} by providing facilities to copy {@link Parameter} descriptions from
 * the {@link #dummyWithParameterAnnotations()} method.
 */
public abstract class SharedParametersAdapter implements SharedParameters {

    private final List<Parameter> parameters = new ArrayList<>();

    protected SharedParametersAdapter() {
        try {
            parameters.addAll(Arrays.stream(getClass().getMethod("dummyWithParameterAnnotations")
                                                      .getAnnotationsByType(Parameter.class)).toList());
        } catch (Exception e) {
            Exceptions.handle(e);
        }
    }

    /**
     * An empty dummy method that is only used for annotating {@link Parameter} descriptions.
     */
    public abstract void dummyWithParameterAnnotations();

    @Override
    public List<Parameter> getParameters() {
        return Collections.unmodifiableList(parameters);
    }
}
