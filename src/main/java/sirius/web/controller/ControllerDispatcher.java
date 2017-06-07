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
import sirius.kernel.di.std.PriorityParts;
import sirius.kernel.di.std.Register;
import sirius.kernel.health.Exceptions;
import sirius.kernel.health.Log;
import sirius.kernel.nls.NLS;
import sirius.web.ErrorCodeException;
import sirius.web.http.InputStreamHandler;
import sirius.web.http.WebContext;
import sirius.web.http.WebDispatcher;
import sirius.web.security.UserContext;
import sirius.web.security.UserInfo;
import sirius.web.services.JSONStructuredOutput;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.channels.ClosedChannelException;
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

    @PriorityParts(Interceptor.class)
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
            if (tryExecuteRoute(ctx, preDispatch, uri, route)) {
                return true;
            }
        }
        return false;
    }

    private boolean tryExecuteRoute(WebContext ctx, boolean preDispatch, String uri, Route route) {
        try {
            final List<Object> params = route.matches(ctx, uri, preDispatch);
            if (params == null) {
                // Route did not match...
                return false;
            }

            // Check if interceptors permit execution of route...
            for (Interceptor interceptor : interceptors) {
                if (!interceptor.shouldExecuteRoute(ctx, route.isJSONCall(), route.getController())) {
                    return false;
                }
            }

            // Inject WebContext as first parameter...
            params.add(0, ctx);

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
                 .fork(() -> performRouteInOwnThread(ctx, route, params));
            return true;
        } catch (final Throwable e) {
            tasks.executor("web-mvc")
                 .dropOnOverload(() -> ctx.respondWith()
                                          .error(HttpResponseStatus.INTERNAL_SERVER_ERROR,
                                                 "Request dropped - System overload!"))
                 .fork(() -> handleFailure(ctx, route, e));
            return true;
        }
    }

    private void performRouteInOwnThread(WebContext ctx, Route route, List<Object> params) {
        try {
            setupContext(ctx, route);

            // Intercept call...
            for (Interceptor interceptor : interceptors) {
                if (interceptor.before(ctx, route.isJSONCall(), route.getController(), route.getSuccessCallback())) {
                    return;
                }
            }

            // Install user. This is forcefully called here to ensure that the ScopeDetetor
            // and the user manager are guaranteed to be invoked one we enter the controller code...
            UserInfo user = UserContext.getCurrentUser();

            // If the underlying ScopeDetector made a redirect (for whatever reasons)
            // the response will be committed and we can (must) safely return...
            if (ctx.isResponseCommitted()) {
                return;
            }

            // If a user authenticated during this call...bind to session!
            if (user.isLoggedIn()) {
                UserContext.get().attachUserToSession();
            }

            String missingPermission = route.checkAuth(user);
            if (missingPermission != null) {
                handlePermissionError(ctx, route, missingPermission);
            } else {
                executeRoute(ctx, route, params);
            }
        } catch (InvocationTargetException ex) {
            handleFailure(ctx, route, ex.getTargetException());
        } catch (ClosedChannelException ex) {
            // Especially a JSON call might re-throw this. As this simply states, the connection was
            // closed while writing JSON data, we can safely ignore it....
            Exceptions.ignore(ex);
        } catch (Throwable ex) {
            handleFailure(ctx, route, ex);
        }
        ctx.enableTiming(route.toString());
    }

    private void executeRoute(WebContext ctx, Route route, List<Object> params) throws Exception {
        if (route.isJSONCall()) {
            executeJSONCall(ctx, route, params);
        } else {
            route.getSuccessCallback().invoke(route.getController(), params.toArray());
        }
    }

    private void executeJSONCall(WebContext ctx, Route route, List<Object> params) throws Exception {
        JSONStructuredOutput out = ctx.respondWith().json();
        params.add(1, out);
        out.beginResult();
        out.property("success", true);
        out.property("error", false);
        route.getSuccessCallback().invoke(route.getController(), params.toArray());
        out.endResult();
    }

    private void handlePermissionError(WebContext ctx, Route route, String missingPermission) throws Exception {
        for (Interceptor interceptor : interceptors) {
            if (interceptor.beforePermissionError(missingPermission,
                                                  ctx,
                                                  route.isJSONCall(),
                                                  route.getController(),
                                                  route.getSuccessCallback())) {
                return;
            }
        }

        if (route.isJSONCall()) {
            throw Exceptions.createHandled()
                            .withSystemErrorMessage("Missing permission: %s", missingPermission)
                            .handle();
        }

        // No Interceptor is in charge...report error...
        ctx.respondWith().error(HttpResponseStatus.UNAUTHORIZED);
    }

    private void setupContext(WebContext ctx, Route route) {
        CallContext.getCurrent().setLang(NLS.makeLang(ctx.getLang()));
        TaskContext.get()
                   .setSystem(SYSTEM_MVC)
                   .setSubSystem(route.getController().getClass().getSimpleName())
                   .setJob(ctx.getRequestedURI());
    }

    private void handleFailure(WebContext ctx, Route route, Throwable ex) {
        try {
            CallContext.getCurrent()
                       .addToMDC("controller",
                                 route.getController().getClass().getName() + "." + route.getSuccessCallback()
                                                                                         .getName());
            if (route.isJSONCall()) {
                if (ctx.isResponseCommitted()) {
                    // Force underlying request / response to be closed...
                    ctx.respondWith()
                       .error(HttpResponseStatus.INTERNAL_SERVER_ERROR,
                              Exceptions.handle(ControllerDispatcher.LOG, ex));
                    return;
                }

                JSONStructuredOutput out = ctx.respondWith().json();
                out.beginResult();
                out.property("success", false);
                out.property("error", true);
                if (ex instanceof ErrorCodeException) {
                    out.property("code", ((ErrorCodeException) ex).getCode());
                    out.property("message", ex.getMessage());
                } else {
                    out.property("message", Exceptions.handle(ControllerDispatcher.LOG, ex).getMessage());
                }
                out.endResult();
            } else {
                route.getController().onError(ctx, Exceptions.handle(ControllerDispatcher.LOG, ex));
            }
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
