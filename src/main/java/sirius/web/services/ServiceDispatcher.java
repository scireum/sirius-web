/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.services;

import io.netty.handler.codec.http.HttpResponseStatus;
import sirius.kernel.async.CallContext;
import sirius.kernel.async.TaskContext;
import sirius.kernel.async.Tasks;
import sirius.kernel.commons.PriorityCollector;
import sirius.kernel.commons.Strings;
import sirius.kernel.commons.Tuple;
import sirius.kernel.di.GlobalContext;
import sirius.kernel.di.std.Part;
import sirius.kernel.di.std.Register;
import sirius.kernel.nls.NLS;
import sirius.web.http.WebContext;
import sirius.web.http.WebDispatcher;
import sirius.web.security.Permissions;
import sirius.web.security.UserContext;
import sirius.web.security.UserInfo;

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

    @Override
    public int getPriority() {
        return PriorityCollector.DEFAULT_PRIORITY - 5;
    }

    @Part
    private GlobalContext gc;

    @Part
    private Tasks tasks;

    @Override
    public boolean preDispatch(WebContext ctx) throws Exception {
        return false;
    }

    @Override
    public boolean dispatch(final WebContext ctx) throws Exception {
        if (!ctx.getRequest().uri().startsWith("/service/")) {
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

        tasks.executor("web-services")
             .dropOnOverload(() -> ctx.respondWith()
                                      .error(HttpResponseStatus.INTERNAL_SERVER_ERROR,
                                             "Request dropped - System overload!"))
             .fork(() -> invokeService(ctx, handler.getFirst(), handler.getSecond()));
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

        // Install language
        CallContext.getCurrent().setLang(NLS.makeLang(ctx.getLang()));

        // Install user. This is forcefully called here to ensure that the ScopeDetetor
        // and the user manager are guaranteed to be invoked one we enter the service code...
        UserInfo user = UserContext.getCurrentUser();

        // If the underlying ScopeDetector made a redirect (for whatever reasons)
        // the response will be committed and we can (must) safely return...
        if (ctx.isResponseCommitted()) {
            return;
        }

        // ... and check permissions
        for (String p : Permissions.computePermissionsFromAnnotations(serv.getClass())) {
            if (!user.hasPermission(p)) {
                ctx.respondWith().error(HttpResponseStatus.UNAUTHORIZED, "Missing permission: " + p);
                return;
            }
        }

        ctx.enableTiming(null);

        call.invoke(serv);
    }
}
