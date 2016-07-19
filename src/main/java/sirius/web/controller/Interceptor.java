/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.controller;

import sirius.kernel.di.std.Priorized;
import sirius.web.http.WebContext;

import java.lang.reflect.Method;

/**
 * Can be used to intercept calls to controllers ({@link Controller})
 */
public interface Interceptor extends Priorized {
    /**
     * Invoked before the call to the given method would be performed.
     *
     * @param ctx        provides access to the current web context
     * @param jsonCall   determines if this is a JSON call or a regular wb request (probably serving html).
     * @param controller the controller which is active
     * @param method     the method which will be called
     * @return <tt>true</tt> if the call is handled by the interceptor, <tt>false</tt> if the method should be
     * invoked
     * @throws java.lang.Exception in case of an error. Throw a {@link sirius.kernel.health.HandledException} to
     *                             signal, that all logging and handling has already been performed.
     *                             Any other exception will be logged and reported as system error.
     */
    boolean before(WebContext ctx, boolean jsonCall, Controller controller, Method method) throws Exception;

    /**
     * Invoked before the request is aborted due to a missing permission.
     *
     * @param permission the permission which wasn't granted to the user and therefore caused the error
     * @param ctx        provides access to the current web context
     * @param jsonCall   determines if this is a JSON call or a regular wb request (probably serving html).
     * @param controller the controller which is active
     * @param method     the method which would have been called
     * @return <tt>true</tt> if the call is handled by the interceptor, <tt>false</tt> if a generic error should be
     * created
     * @throws java.lang.Exception in case of an error. Throw a {@link sirius.kernel.health.HandledException} to
     *                             signal, that all logging and handling has already been performed.
     *                             Any other exception will be logged and reported as system error.
     */
    boolean beforePermissionError(String permission,
                                  WebContext ctx,
                                  boolean jsonCall,
                                  Controller controller,
                                  Method method) throws Exception;

    @Override
    default int getPriority() {
        return DEFAULT_PRIORITY;
    }

    /**
     * Determines if a matched routing should be executed.
     * <p>
     * By default this should most probably return <tt>true</tt> to invoke the {@link Controller}. However,
     * <tt>false</tt> can be returned to skip this routing.
     *
     * @param ctx        the current request
     * @param jsonCall   determines if this is a JSON call (<tt>true</tt>) or a plain request
     * @param controller the controller which contains the matching route
     * @return <tt>true</tt> if the route should be executed, <tt>false</tt> otherwise
     */
    boolean shouldExecuteRoute(WebContext ctx, boolean jsonCall, Controller controller);
}
