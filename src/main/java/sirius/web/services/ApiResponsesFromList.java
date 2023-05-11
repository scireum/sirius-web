/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.services;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Container annotation for {@link ApiResponsesFrom} annotations.
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.METHOD)
public @interface ApiResponsesFromList {
    /**
     * Contains all shared api responses collections assigned to a method.
     *
     * @return all shared api responses collections assigned to a method
     */
    ApiResponsesFrom[] value();
}
