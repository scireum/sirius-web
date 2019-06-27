/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.services;

import io.netty.handler.codec.http.HttpResponseStatus;
import sirius.kernel.async.TaskContext;
import sirius.kernel.commons.CachingSupplier;
import sirius.kernel.commons.PriorityCollector;
import sirius.kernel.commons.Strings;
import sirius.kernel.commons.Tuple;
import sirius.kernel.di.GlobalContext;
import sirius.kernel.di.std.Part;
import sirius.kernel.di.std.Register;
import sirius.web.http.Firewall;
import sirius.web.http.Limited;
import sirius.web.http.Unlimited;
import sirius.web.http.WebContext;
import sirius.web.http.WebDispatcher;
import sirius.web.security.Permissions;
import sirius.web.security.UserContext;
import sirius.web.security.UserInfo;

import java.util.Optional;

/**
 * Dispatches calls to the JSON / XML Service-Framework (/service).
 * <p>
 * Processes calls to <tt>/service/[format]/service-name</tt>, by dispatching them to the appropriate
 * {@link StructuredService} and selecting the matching {@link sirius.kernel.xml.StructuredOutput} based on the given
 * <tt>format</tt> (either json or xml).
 */
@Register
public class ServiceDispatcher implements WebDispatcher {

    private static final String SYSTEM_SERVICE = "SERVICE";

    @Part
    private GlobalContext gc;

    @Part
    private Firewall firewall;

    @Override
    public int getPriority() {
        return PriorityCollector.DEFAULT_PRIORITY - 5;
    }

    @Override
    public boolean dispatch(final WebContext ctx) throws Exception {
        if (!ctx.getRequestedURI().startsWith("/service/")) {
            return false;
        }
        // The real dispatching is put into its own method to support inlining of this check by the JIT
        return doDispatch(ctx);
    }

    /*
     * Actually tries to dispatch the incoming request which starts with /service....
     */
    private boolean doDispatch(WebContext ctx) {
        String uri = ctx.getRequestedURI();
        Tuple<ServiceCall, StructuredService> handler = parsePath(ctx, uri);
        if (handler.getSecond() == null) {
            return false;
        }

        invokeService(ctx, handler.getFirst(), handler.getSecond());
        return true;
    }

    private Tuple<ServiceCall, StructuredService> parsePath(WebContext ctx, String uri) {
        // Cut /service/
        final String subpath = uri.substring(9);

        Tuple<String, String> callPath = Strings.split(subpath, "/");
        String type = callPath.getFirst();
        String service = callPath.getSecond();
        ServiceCall call = null;
        if ("xml".equals(type)) {
            call = new XMLServiceCall(ctx);
        } else if ("json".equals(type)) {
            call = new JSONServiceCall(ctx);
        } else {
            if (Strings.isFilled(service)) {
                service = type + "/" + service;
            } else {
                service = type;
            }
            call = new RawServiceCall(ctx);
        }
        StructuredService serv = gc.getPart(service, StructuredService.class);

        return Tuple.create(call, serv);
    }

    private void invokeService(WebContext ctx, ServiceCall call, StructuredService serv) {
        TaskContext.get().setSystem(SYSTEM_SERVICE).setSubSystem(serv.getClass().getSimpleName());

        // Check firewall
        if (firewall != null && !serv.getClass().isAnnotationPresent(Unlimited.class)) {
            if (firewall.handleRateLimiting(ctx,
                                            Optional.ofNullable(serv.getClass().getAnnotation(Limited.class))
                                                    .map(Limited::value)
                                                    .orElse(Limited.HTTP))) {
                return;
            }
        }

        // ... and check permissions
        CachingSupplier<UserInfo> userSupplier = new CachingSupplier<>(UserContext::getCurrentUser);
        for (String p : Permissions.computePermissionsFromAnnotations(serv.getClass())) {
            if (!userSupplier.get().hasPermission(p)) {
                ctx.respondWith().error(HttpResponseStatus.UNAUTHORIZED, "Missing permission: " + p);
                return;
            }
        }

        ctx.enableTiming(null);

        call.invoke(serv);
    }
}
