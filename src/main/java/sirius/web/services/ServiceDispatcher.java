/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.services;

import io.netty.handler.codec.http.HttpResponseStatus;
import sirius.kernel.async.Async;
import sirius.kernel.commons.*;
import sirius.kernel.di.GlobalContext;
import sirius.kernel.di.std.Context;
import sirius.kernel.di.std.Register;
import sirius.kernel.health.Exceptions;
import sirius.web.http.WebContext;
import sirius.web.http.WebDispatcher;
import sirius.web.security.Permissions;
import sirius.web.security.UserContext;

import java.util.Collection;
import java.util.List;

/**
 * Dispatches calls to the JSON / XML Service-Framework (/service).
 * <p>
 * Processes calls to <tt>/service/[format]/service-name</tt>, by dispatching them to the appropriate
 * {@link StructuredService} and selecting the matching {@link sirius.kernel.xml.StructuredOutput} based on the given
 * <tt>format</tt> (either json or xml).
 *
 * @author Andreas Haufler (aha@scireum.de)
 * @since 2013/11
 */
@Register
public class ServiceDispatcher implements WebDispatcher {

    @Override
    public int getPriority() {
        return PriorityCollector.DEFAULT_PRIORITY;
    }

    @Context
    private GlobalContext gc;

    @Override
    public boolean preDispatch(WebContext ctx) throws Exception {
        return false;
    }

    @Override
    public boolean dispatch(final WebContext ctx) throws Exception {
        // We use the translated URI because legacy /services might have been routed elsewhere.
        if (!ctx.getRequestedURI().startsWith("/service")) {
            return false;
        }
        String uri = ctx.getRequestedURI();
        if ("/service".equals(uri)) {
            if (ctx.get("service").isFilled()) {
                StructuredService service = gc.getPart(ctx.get("service").asString(), StructuredService.class);
                if (service != null && service.getClass().isAnnotationPresent(AutoDoc.class)) {
                    ctx.respondWith()
                       .cached()
                       .template("/help/service/service.html",
                                 ctx.get("service").asString(),
                                 service.getClass().getAnnotation(AutoDoc.class));
                    return true;
                }
            }
            List<ComparableTuple<String, Collection<StructuredService>>> allDocumentedServices = collectServiceInfo();
            ctx.respondWith().cached().template("/help/service/info.html", allDocumentedServices);
            return true;
        }
        // Cut /service/
        final String subpath = uri.substring(9);
        Async.executor("web-services")
             .fork(() -> performServiceCall(ctx, subpath))
             .dropOnOverload(() -> ctx.respondWith()
                                      .error(HttpResponseStatus.INTERNAL_SERVER_ERROR,
                                             "Request dropped - System overload!"))
             .execute();
        return true;
    }

    private void performServiceCall(WebContext ctx, String subpath) {
        ServiceCall call = null;
        Tuple<String, String> callPath = Strings.split(subpath, "/");
        String type = callPath.getFirst();
        String service = callPath.getSecond();
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

        if (call == null) {
            ctx.respondWith()
               .error(HttpResponseStatus.UNSUPPORTED_MEDIA_TYPE,
                      Exceptions.createHandled()
                                .withSystemErrorMessage("Unknown or unsupported type: %s. Use 'xml' or 'json'", type)
                                .handle());
            return;
        }

        StructuredService serv = gc.getPart(service, StructuredService.class);
        if (serv == null) {
            call.handle(null,
                        Exceptions.createHandled()
                                  .withSystemErrorMessage(
                                          "Unknown service: %s. Try /services for a complete list of available services.",
                                          service)
                                  .handle());
        } else {
            for (String p : Permissions.computePermissionsFromAnnotations(serv.getClass())) {
                if (!UserContext.getCurrentUser().hasPermission(p)) {
                    ctx.respondWith().error(HttpResponseStatus.UNAUTHORIZED, "Missing permission: " + p);
                    return;
                }
            }
            call.invoke(serv);
        }
    }

    private List<ComparableTuple<String, Collection<StructuredService>>> collectServiceInfo() {
        MultiMap<String, StructuredService> result = MultiMap.create();
        for (StructuredService ss : gc.getParts(StructuredService.class)) {
            AutoDoc ad = ss.getClass().getAnnotation(AutoDoc.class);
            if (ad != null) {
                result.put(ad.category(), ss);
            }
        }
        return ComparableTuple.fromComparableMap(result.getUnderlyingMap());
    }

}
