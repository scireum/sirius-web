/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.security;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Boilerplate annotation for {@code {@literal @}Permission(UserInfo.PERMISSION_LOGGED_IN)}.
 *
 * @see sirius.web.security.Permission
 * @see sirius.web.security.UserInfo#PERMISSION_LOGGED_IN
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.METHOD)
public @interface LoginRequired {
}
