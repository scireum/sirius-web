/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.controller;

import io.netty.handler.codec.http.HttpResponseStatus;
import sirius.kernel.async.CallContext;
import sirius.kernel.async.Promise;
import sirius.kernel.async.TaskContext;
import sirius.kernel.async.Tasks;
import sirius.kernel.commons.CachingSupplier;
import sirius.kernel.commons.Callback;
import sirius.kernel.commons.Explain;
import sirius.kernel.commons.PriorityCollector;
import sirius.kernel.di.Injector;
import sirius.kernel.di.std.Part;
import sirius.kernel.di.std.PriorityParts;
import sirius.kernel.di.std.Register;
import sirius.kernel.health.Exceptions;
import sirius.kernel.health.Log;
import sirius.kernel.xml.StructuredOutput;
import sirius.web.services.Format;
import sirius.web.http.Firewall;
import sirius.web.http.InputStreamHandler;
import sirius.web.http.Limited;
import sirius.web.http.Unlimited;
import sirius.web.http.WebContext;
import sirius.web.http.WebDispatcher;
import sirius.web.security.UserContext;

import javax.annotation.Nullable;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.channels.ClosedChannelException;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

/**
 * Dispatches incoming requests to the appropriate {@link Controller}.
 */
@Register(classes = {WebDispatcher.class, ControllerDispatcher.class})
public class ControllerDispatcher implements WebDispatcher {

    protected static final Log LOG = Log.get("controller");
    private static final String SYSTEM_MVC = "MVC";

    /**
     * Contains the pattern defined by the route being matched.
     * <p>
     * This will be put into {@link WebContext#setAttribute(String, Object)}.
     */
    public static final String ATTRIBUTE_MATCHED_ROUTE = "sirius_matchedRoute";

    private List<Route> routes;

    @PriorityParts(Interceptor.class)
    private Collection<Interceptor> interceptors;

    @Part
    private Tasks tasks;

    @Part
    @Nullable
    private Firewall firewall;

    /**
     * The priority of this controller is {@code PriorityCollector.DEFAULT_PRIORITY + 10} as it is quite complex
     * to check each request against each route.
     *
     * @return the priority of this dispatcher
     */
    @Override
    public int getPriority() {
        return PriorityCollector.DEFAULT_PRIORITY + 10;
    }

    @Override
    @SuppressWarnings("squid:S1698")
    @Explain("We actually can use object identity here as this is a marker object.")
    public Callback<WebContext> preparePreDispatch(WebContext ctx) {
        String uri = determineEffectiveURI(ctx);
        for (final Route route : getRoutes()) {
            final List<Object> params = shouldExecute(ctx, uri, route, true);
            if (params != Route.NO_MATCH) {
                InputStreamHandler handler = new InputStreamHandler();
                ctx.setContentHandler(handler);

                return newCtx -> preparePerformRoute(newCtx, route, params, handler);
            }
        }

        return null;
    }

    private String determineEffectiveURI(WebContext ctx) {
        String uri = ctx.getRawRequestedURI();
        if (uri.endsWith("/") && !"/".equals(uri)) {
            uri = uri.substring(0, uri.length() - 1);
        }
        return uri;
    }

    @SuppressWarnings("squid:S1698")
    @Explain("We actually can use object identity here as this is a marker object.")
    private List<Object> shouldExecute(WebContext ctx, String uri, Route route, boolean preDispatch) {
        final List<Object> params = route.matches(ctx, uri, preDispatch);
        if (params == Route.NO_MATCH) {
            // Route did not match...
            return params;
        }
        // Check if interceptors permit execution of route...
        for (Interceptor interceptor : interceptors) {
            if (!interceptor.shouldExecuteRoute(ctx, route)) {
                return Route.NO_MATCH;
            }
        }

        return params;
    }

    private void preparePerformRoute(WebContext ctx,
                                     Route route,
                                     List<Object> params,
                                     InputStreamHandler inputStreamHandler) {
        try {
            if (firewall != null
                && !route.getMethod().isAnnotationPresent(Unlimited.class)
                && firewall.handleRateLimiting(ctx,
                                               Optional.ofNullable(route.getMethod().getAnnotation(Limited.class))
                                                       .map(Limited::value)
                                                       .orElse(Limited.HTTP))) {
                return;
            }

            // Inject WebContext as first parameter...
            params.add(0, ctx);

            // If a route is pre-dispatchable we inject an InputStream as last parameter of the
            // call. This is also checked by the route-compiler
            if (inputStreamHandler != null) {
                params.add(inputStreamHandler);
            }
            performRoute(ctx, route, params);
        } catch (final Exception e) {
            handleFailure(ctx, route, e);
        }
    }

    @Override
    @SuppressWarnings("squid:S1698")
    @Explain("We actually can use object identity here as this is a marker object.")
    public DispatchDecision dispatch(WebContext ctx) throws Exception {
        String uri = determineEffectiveURI(ctx);
        for (final Route route : getRoutes()) {
            final List<Object> params = shouldExecute(ctx, uri, route, false);
            if (params != Route.NO_MATCH) {
                preparePerformRoute(ctx, route, params, null);
                return DispatchDecision.DONE;
            }
        }

        return DispatchDecision.CONTINUE;
    }

    private void performRoute(WebContext ctx, Route route, List<Object> params) {
        try {
            TaskContext.get()
                       .setSystem(SYSTEM_MVC)
                       .setSubSystem(route.getController().getClass().getSimpleName())
                       .setJob(ctx.getRequestedURI());

            // Intercept call...
            for (Interceptor interceptor : interceptors) {
                if (interceptor.before(ctx, route)) {
                    return;
                }
            }

            String missingPermission = route.checkAuth(new CachingSupplier<>(UserContext::getCurrentUser));

            if (missingPermission != null) {
                handlePermissionError(ctx, route, missingPermission);
            } else {
                executeRoute(ctx, route, params);
            }
        } catch (InvocationTargetException ex) {
            handleFailure(ctx, route, ex.getTargetException());
        } catch (ClosedChannelException ex) {
            // Especially a service call might re-throw this. As this simply states, the connection was
            // closed while writing data, we can safely ignore it....
            Exceptions.ignore(ex);
        } catch (Exception ex) {
            handleFailure(ctx, route, ex);
        }
        ctx.enableTiming(route.toString());
    }

    private void executeRoute(WebContext ctx, Route route, List<Object> params) throws Exception {
        ctx.setAttribute(ATTRIBUTE_MATCHED_ROUTE, route.getPattern());
        if (route.getApiResponseFormat() == Format.JSON) {
            executeApiCall(ctx, route, ctx.respondWith().json(), params);
        } else if (route.getApiResponseFormat() == Format.XML) {
            executeApiCall(ctx, route, ctx.respondWith().xml(), params);
        } else {
            route.invoke(params);
        }
    }

    private void executeApiCall(WebContext ctx, Route route, StructuredOutput out, List<Object> params)
            throws Exception {
        params.add(1, out);
        out.beginResult();
        out.property("success", true);
        out.property("error", false);
        Object result = route.invoke(params);
        if (result instanceof Promise) {
            ((Promise<?>) result).onSuccess(ignored -> out.endResult()).onFailure(e -> {
                handleFailure(ctx, route, e);
            });
        } else {
            out.endResult();
        }
    }

    private void handlePermissionError(WebContext ctx, Route route, String missingPermission) throws Exception {
        for (Interceptor interceptor : interceptors) {
            if (interceptor.beforePermissionError(missingPermission, ctx, route)) {
                return;
            }
        }

        if (route.isServiceCall()) {
            throw Exceptions.createHandled()
                            .withSystemErrorMessage("Missing permission: %s", missingPermission)
                            .handle();
        }

        // No Interceptor is in charge...report error...
        ctx.respondWith().error(HttpResponseStatus.UNAUTHORIZED);
    }

    private void handleFailure(WebContext ctx, Route route, Throwable ex) {
        try {
            // We never want to log or handle exceptions which are caused by the user which
            // closed the browser / socket mid processing...
            if (ex instanceof ClosedChannelException && ctx.isResponseCommitted()) {
                Exceptions.ignore(ex);
                return;
            }

            CallContext.getCurrent()
                       .addToMDC("controller",
                                 route.getController().getClass().getName() + "." + route.getMethod().getName());
            if (route.isServiceCall()) {
                route.getController().onApiError(ctx, Exceptions.handle(LOG, ex), route.getApiResponseFormat());
            } else {
                route.getController().onError(ctx, Exceptions.handle(LOG, ex));
            }
        } catch (Exception t) {
            ctx.respondWith()
               .error(HttpResponseStatus.INTERNAL_SERVER_ERROR, Exceptions.handle(ControllerDispatcher.LOG, t));
        }
    }

    /*
     * Compiles all available controllers and their methods into a route table
     */
    private List<Route> buildRouter() {
        PriorityCollector<Route> collector = PriorityCollector.create();
        for (Controller controller : Injector.context().getParts(Controller.class)) {
            compileController(collector, controller);
        }

        List<Route> allRoutes = collector.getData();
        optimizeRoutes(allRoutes);

        return allRoutes;
    }

    private void optimizeRoutes(List<Route> routes) {
        for (int i = 0; i < routes.size() - 1; i++) {
            Route baseRoute = routes.get(i);
            int priority = baseRoute.getMethod().getAnnotation(Routed.class).priority();
            scanRoutesWithSamePriority(routes, baseRoute, i, priority);
        }
    }

    private void scanRoutesWithSamePriority(List<Route> routes, Route baseRoute, int baseIndex, int basePriority) {
        int index = baseIndex + 1;
        while (index < routes.size()) {
            Route secondRoute = routes.get(index);
            if (secondRoute.getMethod().getAnnotation(Routed.class).priority() > basePriority) {
                return;
            }

            if (secondRoute.getPattern().equals(baseRoute.getPattern())
                && secondRoute.isPreDispatchable() == baseRoute.isPreDispatchable()) {
                if (secondRoute.getMethod().equals(baseRoute.getMethod())) {
                    routes.remove(index);
                    index--;
                    LOG.FINE("Removing duplicate route entry for '%s' in controller '%s'. "
                             + "This is probably a parent class to several controllers",
                             baseRoute.getMethod().getName(),
                             baseRoute.getMethod().getDeclaringClass().getName(),
                             secondRoute.getMethod().getName(),
                             secondRoute.getMethod().getDeclaringClass().getName());
                } else {
                    LOG.WARN("Route collision for '%s' in controller '%s' and '%s' in controller '%s'. "
                             + "Please provide a priority to resolve this!",
                             baseRoute.getMethod().getName(),
                             baseRoute.getMethod().getDeclaringClass().getName(),
                             secondRoute.getMethod().getName(),
                             secondRoute.getMethod().getDeclaringClass().getName());
                }
            }

            index++;
        }
    }

    private void compileController(PriorityCollector<Route> collector, Controller controller) {
        for (final Method m : controller.getClass().getMethods()) {
            if (m.isAnnotationPresent(Routed.class)) {
                Routed routed = m.getAnnotation(Routed.class);
                Route route = compileMethod(routed, controller, m);
                if (route != null) {
                    collector.add(routed.priority(), route);
                }
            }
        }
    }

    /**
     * Returns a list of all {@link Route routes} known to the dispatcher.
     *
     * @return a list of all routes known to the dispatcher
     */
    public List<Route> getRoutes() {
        if (routes == null) {
            routes = buildRouter();
        }

        return Collections.unmodifiableList(routes);
    }

    /*
     * Compiles a method wearing a Routed annotation.
     */
    private Route compileMethod(Routed routed, final Controller controller, final Method m) {
        try {
            return Route.compile(controller, m, routed);
        } catch (Exception e) {
            LOG.WARN("Skipping '%s' in controller '%s' - Cannot compile route '%s': %s (%s)",
                     m.getName(),
                     controller.getClass().getName(),
                     routed.value(),
                     e.getMessage(),
                     e.getClass().getName());
            return null;
        }
    }
}
