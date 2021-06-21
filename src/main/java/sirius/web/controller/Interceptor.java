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

/**
 * Can be used to intercept calls to controllers ({@link Controller})
 */
public interface Interceptor extends Priorized {
    /**
     * Invoked before the call to the given method would be performed.
     *
     * @param ctx   provides access to the current web context
     * @param route the route which is about to be executed
     * @return <tt>true</tt> if the call is handled by the interceptor, <tt>false</tt> if the method should be
     * invoked
     * @throws java.lang.Exception in case of an error. Throw a {@link sirius.kernel.health.HandledException} to
     *                             signal, that all logging and handling has already been performed.
     *                             Any other exception will be logged and reported as system error.
     */
    boolean before(WebContext ctx, Route route) throws Exception;

    /**
     * Invoked before the request is aborted due to a missing permission.
     *
     * @param permission the permission which wasn't granted to the user and therefore caused the error
     * @param ctx        provides access to the current web context
     * @param route      the route which yielded the permission error
     * @return <tt>true</tt> if the call is handled by the interceptor, <tt>false</tt> if a generic error should be
     * created
     * @throws java.lang.Exception in case of an error. Throw a {@link sirius.kernel.health.HandledException} to
     *                             signal, that all logging and handling has already been performed.
     *                             Any other exception will be logged and reported as system error.
     */
    boolean beforePermissionError(String permission, WebContext ctx, Route route) throws Exception;

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
     * @param ctx   the current request
     * @param route the route about to be executed
     * @return <tt>true</tt> if the route should be executed, <tt>false</tt> otherwise
     */
    boolean shouldExecuteRoute(WebContext ctx, Route route);
}
