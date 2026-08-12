/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.controller;

import sirius.kernel.di.std.Priorized;
import sirius.web.cors.AllowedOrigin;
import sirius.web.http.WebContext;

import java.util.Collection;
import java.util.Optional;

/**
 * Can be used to intercept calls to controllers ({@link Controller})
 */
public interface Interceptor extends Priorized {

    /**
     * Determines if processing the given request should be handled normally or if a separate thread pool is required.
     * <p>
     * Some requests are overly complex to process and might slow down or even block the whole webserver. Therefore,
     * throwing these requests into a separate thread pool with proper limits and a drop handler can greatly
     * improve system stability and resilience.
     * <p>
     * Therefore, this method is invoked to inspect each given request. If the request should be handled normally,
     * <tt>false</tt> has to be returned. Otherwise, handling can be forked into a new thread pool or the like
     * and once processing is complete, the <tt>continuation</tt> has to be invoked. In this case the method
     * must return <tt>true</tt> to signal, that processing has been shifted elsewhere using the continuation.
     *
     * @param webContext   the current request to process
     * @param route        the route being matched
     * @param continuation the continuation to call if the request is handled in a separate thread
     * @return <tt>false</tt> if the request should be handled in the default thread pool. In this case the given
     * <tt>continuation</tt> <b>must not</b> be called. Or, <tt>true</tt> to signal that another thread is in charge of
     * handling the request, in which case the <tt>continuation</tt> <b>must</b> be called.
     * @throws Exception in case of any error when processing the request
     */
    default boolean fork(WebContext webContext, Route route, Runnable continuation) throws Exception {
        return false;
    }

    /**
     * Invoked before the call to the given method would be performed.
     *
     * @param webContext provides access to the current web context
     * @param route      the route which is about to be executed
     * @return <tt>true</tt> if the call is handled by the interceptor, <tt>false</tt> if the method should be
     * invoked
     * @throws java.lang.Exception in case of an error. Throw a {@link sirius.kernel.health.HandledException} to
     *                             signal, that all logging and handling has already been performed.
     *                             Any other exception will be logged and reported as system error.
     */
    boolean before(WebContext webContext, Route route) throws Exception;

    /**
     * Invoked before the request is aborted due to a missing permission.
     *
     * @param permission the permission which wasn't granted to the user and therefore caused the error
     * @param webContext provides access to the current web context
     * @param route      the route which yielded the permission error
     * @return <tt>true</tt> if the call is handled by the interceptor, <tt>false</tt> if a generic error should be
     * created
     * @throws java.lang.Exception in case of an error. Throw a {@link sirius.kernel.health.HandledException} to
     *                             signal, that all logging and handling has already been performed.
     *                             Any other exception will be logged and reported as system error.
     */
    boolean beforePermissionError(String permission, WebContext webContext, Route route) throws Exception;

    @Override
    default int getPriority() {
        return DEFAULT_PRIORITY;
    }

    /**
     * Determines if a matched routing should be executed.
     * <p>
     * By default, this should most probably return <tt>true</tt> to invoke the {@link Controller}. However,
     * <tt>false</tt> can be returned to skip this routing.
     *
     * @param webContext the current request
     * @param route      the route about to be executed
     * @return <tt>true</tt> if the route should be executed, <tt>false</tt> otherwise
     */
    boolean shouldExecuteRoute(WebContext webContext, Route route);

    /**
     * Tries to determine which CORS origins should be allowed for the given collection of {@link Route}s.
     *
     * <p>
     * In order to determine the correct allowed origin for a given endpoint, this method is invoked with the current
     * WebContext and the collection of routes matching the called URI.
     * The interceptor may then inspect the routes and answer in one of two different ways:
     * <ol>
     *     <li>By returning the preferred origin</li>
     *     <li>By returning an empty optional to signal it is not able to make a decision.</li>
     * </ol>
     * </p>
     *
     * <p>
     * Instead of returning the raw origin directly, the choice is represented in form of a subclass of the sealed
     * {@link AllowedOrigin} interface. The caller must then handle it accordingly, e.g. by resolving to the concrete
     * origin.
     * </p>
     *
     * <p>
     * By <b>default</b>, this method returns an empty optional, signalling that this interceptor does not make a
     * decision. If no interceptor decides on a strategy, the {@link ControllerDispatcher} falls back to an
     * {@link AllowedOrigin.MatchRequest}, which resolves to the origin of the current request.
     * </p>
     *
     * <p>
     * <b>Abstaining is therefore not the same as refusing.</b> As the fallback reflects the origin of the request,
     * an empty optional leaves a route readable cross-origin. An interceptor which wants a route to stay unreachable
     * for foreign origins must say so by returning an {@link AllowedOrigin.Denied}.
     * </p>
     *
     * <p>
     * Please note that {@code enableCors} acts as a master switch: this method is only consulted when
     * {@link WebContext#isCorsEnabled()} returns {@code true} for the request's scope. If CORS handling is disabled,
     * no origin is resolved at all and this method is not invoked.
     * </p>
     *
     * @param webContext the current request
     * @param routes     the routes matching the uri that has been called
     * @return the preferred CORS allowed origin strategy, or an empty optional if this interceptor does not make a
     * decision
     */
    default Optional<AllowedOrigin> determineAllowedCorsOrigin(WebContext webContext, Collection<Route> routes) {
        return Optional.empty();
    }
}
