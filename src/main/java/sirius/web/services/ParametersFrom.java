/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.services;

import java.lang.annotation.ElementType;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Copies a shared set of {@linkplain io.swagger.v3.oas.annotations.Parameter parameters} to a {@linkplain PublicService public service}.
 */
@Target(ElementType.METHOD)
@Retention(RetentionPolicy.RUNTIME)
@Repeatable(ParametersFromList.class)
public @interface ParametersFrom {

    /**
     * Contains the descriptor class documenting the set of shared parameters.
     *
     * @return the descriptor to copy from
     */
    Class<? extends SharedParameters> value();
}
