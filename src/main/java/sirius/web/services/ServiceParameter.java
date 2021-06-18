/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.services;

import javax.annotation.Nullable;
import java.lang.annotation.ElementType;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Describes a parameter of a {@link PublicService}.
 * <p>
 * Note that an annotation should be added for each parameter accepted by the service. These annotations must
 * be placed on the same method next to the <tt>PublicService</tt>.
 */
@Target(ElementType.METHOD)
@Retention(RetentionPolicy.RUNTIME)
@Repeatable(ServiceParameters.class)
public @interface ServiceParameter {
    /**
     * Specifies the parameter name as expected in the POST or query string.
     *
     * @return the name of the parameter
     */
    String name();

    /**
     * Provides a short and concise description of the parameter.
     *
     * @return the parameter description which will be auto translated {@link sirius.kernel.nls.NLS#smartGet(String)}.
     */
    String description();

    /**
     * Determines if the parameter is required or optional.
     *
     * @return <tt>true</tt> if the parameter is required, <tt>false</tt> otherwise
     */
    boolean required();

    /**
     * Provides an example value to be shown in the documentation.
     *
     * @return an example value which could be put into the parameter
     */
    @Nullable String example();
}
