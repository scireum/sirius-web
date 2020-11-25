/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.controller;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Specifies the sub scope to be used for a given route.
 * <p>
 * By default {@link #SUB_SCOPE_UI} is used. If a sub scope is set, the value is enforced via
 * {@link sirius.web.security.UserInfo#isSubScopeEnabled(String)}. However, note that this will only be enforced if
 * the route isn't public (therefore either a {@link sirius.web.security.LoginRequired} or
 * {@link sirius.web.security.Permission} is present.
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.METHOD)
public @interface SubScope {

    /**
     * Contains the name of the default sub scope.
     */
    String SUB_SCOPE_UI = "ui";

    /**
     * Contains the name of the sub scope used to mark the public API.
     * <p>
     * This way an "API" only user can be created (e.g. as done by sirius-biz).
     */
    String SUB_SCOPE_API = "api";

    /**
     * Specifies the sub scope to enforce.
     *
     * @return the name of the sub scope for this route
     */
    String value();
}
