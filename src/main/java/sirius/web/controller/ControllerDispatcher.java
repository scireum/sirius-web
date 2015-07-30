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
import sirius.kernel.async.TaskContext;
import sirius.kernel.async.Tasks;
import sirius.kernel.commons.PriorityCollector;
import sirius.kernel.di.Injector;
import sirius.kernel.di.std.Part;
import sirius.kernel.di.std.Parts;
import sirius.kernel.di.std.Register;
import sirius.kernel.health.Exceptions;
import sirius.kernel.health.Log;
import sirius.kernel.nls.NLS;
import sirius.web.http.InputStreamHandler;
import sirius.web.http.WebContext;
import sirius.web.http.WebDispatcher;
import sirius.web.security.UserContext;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Collection;
import java.util.List;

/**
 * Dispatches incoming requests to the appropriate {@link Controller}.
 */
@Register
public class ControllerDispatcher implements WebDispatcher {

    protected static final Log LOG = Log.get("controller");
    private static final String SYSTEM_MVC = "MVC";

    private List<Route> routes;

    @Parts(Interceptor.class)
    private Collection<Interceptor> interceptors;

    @Part
    private Tasks tasks;

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
    public boolean preDispatch(WebContext ctx) throws Exception {
        if (routes == null) {
            buildRouter();
        }

        return route(ctx, true);
    }

    @Override
    public boolean dispatch(WebContext ctx) throws Exception {
        if (routes == null) {
            buildRouter();
        }

        return route(ctx, false);
    }

    private boolean route(final WebContext ctx, boolean preDispatch) {
        String uri = ctx.getRequestedURI();
        if (uri.endsWith("/") && !"/".equals(uri)) {
            uri = uri.substring(0, uri.length() - 1);
        }
        for (final Route route : routes) {
            try {
                final List<Object> params = route.matches(ctx, uri, preDispatch);
                if (params != null) {
                    // If a route is pre-dispatchable we inject an InputStream as last parameter of the
                    // call. This is also checked by the route-compiler
                    if (preDispatch) {
                        InputStreamHandler ish = new InputStreamHandler();
                        params.add(ish);
                        ctx.setContentHandler(ish);
                    }
                    tasks.executor("web-mvc")
                         .dropOnOverload(() -> ctx.respondWith()
                                                  .error(HttpResponseStatus.INTERNAL_SERVER_ERROR,
                                                         "Request dropped - System overload!"))
                         .fork(performRouteInOwnThread(ctx, route, params));
                    return true;
                }
            } catch (final Throwable e) {
                tasks.executor("web-mvc")
                     .dropOnOverload(() -> ctx.respondWith()
                                              .error(HttpResponseStatus.INTERNAL_SERVER_ERROR,
                                                     "Request dropped - System overload!"))
                     .fork(() -> handleFailure(ctx, route, e));
                return true;
            }
        }
        return false;
    }

    private Runnable performRouteInOwnThread(WebContext ctx, Route route, List<Object> params) {
        return () -> {
            try {
                CallContext.getCurrent().setLang(NLS.makeLang(ctx.getLang()));
                TaskContext.get()
                           .setSystem(SYSTEM_MVC)
                           .setSubSystem(route.getController().getClass().getSimpleName())
                           .setJob(ctx.getRequestedURI());
                params.add(0, ctx);
                // Check if we're allowed to call this route...
                String missingPermission = route.checkAuth();
                if (missingPermission != null) {
                    // No...handle permission error
                    for (Interceptor interceptor : interceptors) {
                        if (interceptor.beforePermissionError(missingPermission,
                                                              ctx,
                                                              route.getController(),
                                                              route.getSuccessCallback())) {
                            return;
                        }
                    }

                    // No Interceptor is in charge...report error...
                    ctx.respondWith().error(HttpResponseStatus.UNAUTHORIZED);
                } else {
                    // Intercept call...
                    for (Interceptor interceptor : interceptors) {
                        if (interceptor.before(ctx, route.getController(), route.getSuccessCallback())) {
                            return;
                        }
                    }
                    // If a user authenticated during this call...bind to session!
                    if (UserContext.getCurrentUser().isLoggedIn()) {
                        CallContext.getCurrent().get(UserContext.class).attachUserToSession();
                    }

                    // Execute routing
                    route.getSuccessCallback().invoke(route.getController(), params.toArray());
                }
            } catch (InvocationTargetException ex) {
                handleFailure(ctx, route, ex.getTargetException());
            } catch (Throwable ex) {
                handleFailure(ctx, route, ex);
            }
            ctx.enableTiming(route.toString());
        };
    }

    private void handleFailure(WebContext ctx, Route route, Throwable ex) {
        try {
            CallContext.getCurrent()
                       .addToMDC("controller",
                                 route.getController().getClass().getName() + "." + route.getSuccessCallback()
                                                                                         .getName());
            route.getController().onError(ctx, Exceptions.handle(ControllerDispatcher.LOG, ex));
        } catch (Throwable t) {
            ctx.respondWith()
               .error(HttpResponseStatus.INTERNAL_SERVER_ERROR, Exceptions.handle(ControllerDispatcher.LOG, t));
        }
    }

    /*
     * Compiles all available controllers and their methods into a route table
     */
    private void buildRouter() {
        PriorityCollector<Route> collector = PriorityCollector.create();
        for (final Controller controller : Injector.context().getParts(Controller.class)) {
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

        routes = collector.getData();
    }

    /*
     * Compiles a method wearing a Routed annotation.
     */
    private Route compileMethod(Routed routed, final Controller controller, final Method m) {
        try {
            final Route route = Route.compile(m, routed);
            route.setPreDispatchable(routed.preDispatchable());
            route.setController(controller);
            route.setSuccessCallback(m);
            return route;
        } catch (Throwable e) {
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
