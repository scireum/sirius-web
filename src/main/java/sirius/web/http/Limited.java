/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.http;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Specifies the realm (counter) to use for rate limiting for this {@link sirius.web.controller.Controller} method
 * or {@link sirius.web.services.StructuredService}
 */
@Documented
@Target({ElementType.METHOD, ElementType.TYPE})
@Retention(RetentionPolicy.RUNTIME)
public @interface Limited {

    /**
     * Represents the default realm used by all HTTP requests which are processed via a controller or service.
     */
    String HTTP = "http";

    /**
     * Determines the realm to use.
     *
     * @return the realm (counter) to used for rate limiting
     */
    String value() default HTTP;
}
