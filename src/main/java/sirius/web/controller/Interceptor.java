/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.controller;

import sirius.web.http.WebContext;

import java.lang.reflect.Method;

/**
 * Can be used to intercept calls to controllers ({@link Controller})
 */
public interface Interceptor {
    /**
     * Invoked before the call to the given method would be performed.
     *
     * @param ctx        provides access to the current web context
     * @param controller the controller which is active
     * @param method     the method which will be called
     * @return <tt>true</tt> if the call is handled by the interceptor, <tt>false</tt> if the method should be
     * invoked
     * @throws java.lang.Exception in case of an error. Throw a {@link sirius.kernel.health.HandledException} to
     *                             signal, that all logging and handling has already been performed.
     *                             Any other exception will be logged and reported as system error.
     */
    boolean before(WebContext ctx, Controller controller, Method method) throws Exception;

    boolean beforePermissionError(String permission, WebContext ctx, Controller controller, Method method)
            throws Exception;
}
