/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.controller;

import io.netty.handler.codec.http.HttpHeaderNames;
import io.netty.handler.codec.http.HttpResponseStatus;
import sirius.kernel.async.CallContext;
import sirius.kernel.async.Promise;
import sirius.kernel.async.TaskContext;
import sirius.kernel.async.Tasks;
import sirius.kernel.commons.CachingSupplier;
import sirius.kernel.commons.Callback;
import sirius.kernel.commons.Explain;
import sirius.kernel.commons.Monoflop;
import sirius.kernel.commons.PriorityCollector;
import sirius.kernel.commons.Strings;
import sirius.kernel.di.Injector;
import sirius.kernel.di.std.ConfigValue;
import sirius.kernel.di.std.Part;
import sirius.kernel.di.std.PriorityParts;
import sirius.kernel.di.std.Register;
import sirius.kernel.health.Exceptions;
import sirius.kernel.health.HandledException;
import sirius.kernel.health.Log;
import sirius.kernel.xml.StructuredOutput;
import sirius.web.http.Firewall;
import sirius.web.http.InputStreamHandler;
import sirius.web.http.Limited;
import sirius.web.http.Unlimited;
import sirius.web.http.WebContext;
import sirius.web.http.WebDispatcher;
import sirius.web.security.MaintenanceInfo;
import sirius.web.security.UserContext;
import sirius.web.security.UserInfo;
import sirius.web.services.Format;
import sirius.web.services.PublicServices;

import javax.annotation.Nullable;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.channels.ClosedChannelException;
import java.time.Duration;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

/**
 * Dispatches incoming requests to the appropriate {@link Controller}.
 */
@Register(classes = {WebDispatcher.class, ControllerDispatcher.class})
public class ControllerDispatcher implements WebDispatcher {

    @ConfigValue("http.maintenanceRetryAfter")
    private static Duration maintenanceRetryAfter;

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
    private List<Interceptor> interceptors;

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
    public Callback<WebContext> preparePreDispatch(WebContext webContext) {
        String uri = determineEffectiveURI(webContext);
        for (final Route route : getRoutes()) {
            final List<Object> params = shouldExecute(webContext, uri, route, true);
            if (params != Route.NO_MATCH) {
                InputStreamHandler handler = new InputStreamHandler();
                webContext.setContentHandler(handler);

                return newCtx -> preparePerformRoute(newCtx, route, params, handler);
            }
        }

        return null;
    }

    private String determineEffectiveURI(WebContext webContext) {
        String uri = webContext.getRawRequestedURI();
        if (uri.endsWith("/") && !"/".equals(uri)) {
            uri = uri.substring(0, uri.length() - 1);
        }
        return uri;
    }

    @SuppressWarnings("squid:S1698")
    @Explain("We actually can use object identity here as this is a marker object.")
    private List<Object> shouldExecute(WebContext webContext, String uri, Route route, boolean preDispatch) {
        final List<Object> params = route.matches(webContext, uri, preDispatch);
        if (params == Route.NO_MATCH) {
            // Route did not match...
            return params;
        }
        // Check if interceptors permit execution of route...
        for (Interceptor interceptor : interceptors) {
            if (!interceptor.shouldExecuteRoute(webContext, route)) {
                return Route.NO_MATCH;
            }
        }

        return params;
    }

    private void preparePerformRoute(WebContext webContext,
                                     Route route,
                                     List<Object> params,
                                     InputStreamHandler inputStreamHandler) {
        try {
            if (firewall != null
                && !route.getMethod().isAnnotationPresent(Unlimited.class)
                && firewall.handleRateLimiting(webContext,
                                               Optional.ofNullable(route.getMethod().getAnnotation(Limited.class))
                                                       .map(Limited::value)
                                                       .orElse(Limited.HTTP))) {
                return;
            }

            // If the route is locked during maintenance, abort in an SEO/user-friendly way of sending an
            // 503 + Retry-After header. We use a generous default timeout here, as this is mostly sufficient..
            if (route.isEnforceMaintenanceMode() && UserContext.getCurrentScope()
                                                               .tryAs(MaintenanceInfo.class)
                                                               .map(MaintenanceInfo::isLocked)
                                                               .orElse(false)) {
                webContext.respondWith()
                          .addHeader(HttpHeaderNames.RETRY_AFTER.toString(), maintenanceRetryAfter.getSeconds())
                          .error(HttpResponseStatus.SERVICE_UNAVAILABLE);
                return;
            }

            // Inject WebContext as first parameter...
            params.add(0, webContext);

            // If a route is pre-dispatchable we inject an InputStream as last parameter of the
            // call. This is also checked by the route-compiler
            if (inputStreamHandler != null) {
                params.add(inputStreamHandler);
            }

            TaskContext.get()
                       .setSystem(SYSTEM_MVC)
                       .setSubSystem(route.getController().getClass().getSimpleName())
                       .setJob(webContext.getRequestedURI());

            performRoute(webContext, route, params, 0);
        } catch (final Exception e) {
            handleFailure(webContext, route, e);
        }
    }

    @Override
    @SuppressWarnings("squid:S1698")
    @Explain("We actually can use object identity here as this is a marker object.")
    public DispatchDecision dispatch(WebContext webContext) throws Exception {
        String uri = determineEffectiveURI(webContext);
        for (final Route route : getRoutes()) {
            final List<Object> params = shouldExecute(webContext, uri, route, false);
            if (params != Route.NO_MATCH) {
                preparePerformRoute(webContext, route, params, null);
                return DispatchDecision.DONE;
            }
        }

        return DispatchDecision.CONTINUE;
    }

    private void performRoute(WebContext webContext, Route route, List<Object> params, int interceptorIndex) {
        try {
            for (int index = interceptorIndex; index < interceptors.size(); index++) {
                final int lastExecutedInterceptor = index;
                if (interceptors.get(index)
                                .fork(webContext,
                                      route,
                                      () -> performRoute(webContext, route, params, lastExecutedInterceptor + 1))) {
                    return;
                }
            }

            for (Interceptor interceptor : interceptors) {
                if (interceptor.before(webContext, route)) {
                    return;
                }
            }

            String missingPermission = route.checkAuth(new CachingSupplier<>(UserContext::getCurrentUser));

            if (missingPermission != null) {
                handlePermissionError(webContext, route, missingPermission);
            } else {
                executeRoute(webContext, route, params);
            }
        } catch (InvocationTargetException ex) {
            handleFailure(webContext, route, ex.getTargetException());
        } catch (Exception ex) {
            handleFailure(webContext, route, ex);
        }
        webContext.enableTiming(route.toString());
    }

    private void executeRoute(WebContext webContext, Route route, List<Object> params) throws Exception {
        webContext.setAttribute(ATTRIBUTE_MATCHED_ROUTE, route.getPattern());

        if (route.getApiResponseFormat() != null) {
            executeApiCall(webContext, route, params);
        } else {
            route.invoke(params);
        }
    }

    private void executeApiCall(WebContext webContext, Route route, List<Object> params) throws Exception {
        StructuredOutput out = createOutput(webContext, route.getApiResponseFormat());
        params.add(1, out);
        out.beginResult();
        out.property("success", true);
        out.property("error", false);
        Object result = route.invoke(params);
        if (result instanceof Promise) {
            ((Promise<?>) result).onSuccess(ignored -> out.endResult())
                                 .onFailure(e -> handleFailure(webContext, route, e));
        } else {
            out.endResult();
        }
    }

    private StructuredOutput createOutput(WebContext webContext, Format apiResponseFormat) {
        return switch (apiResponseFormat) {
            case JSON -> webContext.respondWith().json();
            case XML -> webContext.respondWith().xml();
            default -> throw new IllegalStateException("Unexpected value: " + apiResponseFormat);
        };
    }

    private void handlePermissionError(WebContext webContext, Route route, String missingPermission) throws Exception {
        for (Interceptor interceptor : interceptors) {
            if (interceptor.beforePermissionError(missingPermission, webContext, route)) {
                return;
            }
        }

        if (route.isServiceCall()) {
            if (UserInfo.PERMISSION_LOGGED_IN.equals(missingPermission)) {
                throw Exceptions.createHandled()
                                .withDirectMessage(HttpResponseStatus.UNAUTHORIZED.reasonPhrase())
                                .hint(Controller.HTTP_STATUS, HttpResponseStatus.UNAUTHORIZED.code())
                                .handle();
            } else {
                throw Exceptions.createHandled()
                                .withDirectMessage(Strings.apply("Missing permission: %s", missingPermission))
                                .hint(Controller.HTTP_STATUS, HttpResponseStatus.FORBIDDEN.code())
                                .handle();
            }
        }

        // No Interceptor is in charge...report error...
        webContext.respondWith().error(HttpResponseStatus.UNAUTHORIZED);
    }

    private void handleFailure(WebContext webContext, Route route, Throwable cause) {
        try {
            // We never want to log or handle exceptions which are caused by the user which
            // closed the browser / socket mid-processing...
            if ((cause instanceof ClosedChannelException || cause.getCause() instanceof ClosedChannelException)
                && webContext.isResponseCommitted()) {
                Exceptions.ignore(cause);
                return;
            }

            // A special permission error was detected which is to be handled just like a missing @Permission
            if (cause instanceof HandledException handledException
                && handledException.getHint(Controller.MISSING_PERMISSION).isFilled()) {
                handlePermissionError(webContext,
                                      route,
                                      handledException.getHint(Controller.MISSING_PERMISSION).asString());
                return;
            }

            CallContext.getCurrent()
                       .addToMDC("controller",
                                 route.getController().getClass().getName() + "." + route.getMethod().getName());
            if (route.isServiceCall()) {
                if (cause instanceof HandledException handledException) {
                    route.getController().onApiError(webContext, handledException, route.getApiResponseFormat());
                } else {
                    route.getController()
                         .onApiError(webContext,
                                     Exceptions.handle(LOG, cause)
                                               .withHint(Controller.HTTP_STATUS,
                                                         HttpResponseStatus.INTERNAL_SERVER_ERROR.code()),
                                     route.getApiResponseFormat());
                }
            } else {
                route.getController().onError(webContext, Exceptions.handle(LOG, cause));
            }
        } catch (Exception t) {
            webContext.respondWith()
                      .error(HttpResponseStatus.INTERNAL_SERVER_ERROR, Exceptions.handle(ControllerDispatcher.LOG, t));
        }
    }

    /**
     * Compiles all available controllers and their methods into a route table
     *
     * @return a list holding all available {@link Route routes}
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

    /**
     * Compiles a method wearing a {@link Routed} annotation.
     *
     * @return a {@link Route} matching the annotated criteria
     */
    private Route compileMethod(Routed routed, final Controller controller, final Method method) {
        try {
            return Route.compile(controller, method, routed);
        } catch (Exception e) {
            LOG.WARN("Skipping '%s' in controller '%s' - Cannot compile route '%s': %s (%s)",
                     method.getName(),
                     controller.getClass().getName(),
                     routed.value(),
                     e.getMessage(),
                     e.getClass().getName());
            return null;
        }
    }
}
