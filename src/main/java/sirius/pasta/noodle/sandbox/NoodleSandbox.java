/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.sandbox;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Marks a field / method / class / macro as "safe" to be accessed by user code.
 * <p>
 * As scrips and templates can be created or changed by users, we need a way to determine
 * which methods, macros etc. are safe to call. Therefore, this annotation can be used to mark such places.
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.METHOD, ElementType.TYPE, ElementType.FIELD})
public @interface NoodleSandbox {

    /**
     * Controls the accessibility of the annotated element.
     * <p>
     * <tt>GRANTED</tt> is probably the default case, however, if a whole class is granted, <tt>REJECTED</tt>
     * can be used to overwrite this (as annotations on methods and fields win over ones on classes).
     */
    enum Accessibility {
        GRANTED, REJECTED
    }

    /**
     * Specifies the accessibility value.
     *
     * @return <tt>@{@link Accessibility#GRANTED}</tt> to grant access or @{@link Accessibility#REJECTED} to reject
     * existing access rights.
     */
    Accessibility value();
}
