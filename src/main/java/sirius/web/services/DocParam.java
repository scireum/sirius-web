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
 * Provides a parameter documentation to be used in {@link AutoDoc}
 *
 * @author Andreas Haufler (aha@scireum.de)
 * @since 2013/11
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
public @interface DocParam {
    /**
     * Returns the name of the parameter
     *
     * @return name of the parameter
     */
    String name();

    /**
     * A description for the parameter
     *
     * @return the description for the parameter
     */
    String description() default "";

    /**
     * Determines whether the parameter is required or not
     *
     * @return <tt>true</tt> if the parameter is required, <tt>false</tt> otherwise
     */
    boolean required() default false;
}
