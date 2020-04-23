/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.security;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Expects the absence of the given permission in oder to execute the annotated element.
 * <p>
 * Can be placed on annotation aware elements like implementations of {@link sirius.web.services.StructuredService}
 * or controller methods (implementations of {@link sirius.web.controller.Controller}).
 * <p>
 * Uses {@link UserContext#getCurrentUser()} to obtain the current user and calls
 * {@link sirius.web.security.UserInfo#hasPermission(String)} with the given value prepended with an exclamation mark
 * to check if the current user is authorized.
 *
 * @see sirius.web.security.UserInfo#hasPermission(String)
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.METHOD, ElementType.TYPE})
@Repeatable(NotPermissionList.class)
public @interface NotPermission {
    /**
     * Names the permission which has to be ABSENT for the current user in order to execute the annotated element
     *
     * @return the permission to check
     */
    String value();
}
