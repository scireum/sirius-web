/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.dispatch;

import io.netty.handler.codec.http.HttpHeaderNames;
import io.netty.handler.codec.http.HttpResponseStatus;
import sirius.kernel.commons.Strings;
import sirius.kernel.di.std.ConfigValue;
import sirius.kernel.di.std.Register;
import sirius.web.http.MimeHelper;
import sirius.web.http.WebContext;
import sirius.web.http.WebDispatcher;
import sirius.web.security.UserContext;

/**
 * Sends a 404 (not found) for all unhandled URIs.
 * <p>
 * Also handles some special static URIs if enabled, like /crossdomain.xml or /robots.txt.
 * <p>
 * If no other dispatcher jumps in, this will take care of handing the request by sending a HTTP/404.
 */
@Register
public class DefaultDispatcher implements WebDispatcher {

    @ConfigValue("http.crossdomain.xml.enabled")
    private boolean serveCrossdomain;

    @ConfigValue("http.robots.txt.enabled")
    private boolean serveRobots;

    @ConfigValue("http.robots.txt.disallow")
    private boolean robotsDisallowAll;

    @Override
    public int getPriority() {
        return 999;
    }

    @Override
    public DispatchDecision dispatch(WebContext ctx) throws Exception {
        if ("/crossdomain.xml".equals(ctx.getRequestedURI()) && serveCrossdomain) {
            ctx.respondWith()
               .infinitelyCached()
               .setHeader(HttpHeaderNames.CONTENT_TYPE, MimeHelper.TEXT_XML)
               .direct(HttpResponseStatus.OK,
                       "<?xml version=\"1.0\"?>\n"
                       + "<!DOCTYPE cross-domain-policy SYSTEM "
                       + "\"http://www.adobe.com/xml/dtds/cross-domain-policy.dtd\">\n"
                       + "<cross-domain-policy>\n"
                       + "    <site-control permitted-cross-domain-policies=\"all\" />\n"
                       + "    <allow-access-from domain=\"*\" secure=\"false\" />\n"
                       + "    <allow-http-request-headers-from domain=\"*\" headers=\"*\"/>\n"
                       + "</cross-domain-policy>");
        } else if ("/robots.txt".equals(ctx.getRequestedURI()) && serveRobots) {
            if (robotsDisallowAll) {
                ctx.respondWith()
                   .infinitelyCached()
                   .setHeader(HttpHeaderNames.CONTENT_TYPE, MimeHelper.TEXT_PLAIN)
                   .direct(HttpResponseStatus.OK, "User-agent: *\n" + "Disallow: /\n");
            } else {
                ctx.respondWith()
                   .infinitelyCached()
                   .setHeader(HttpHeaderNames.CONTENT_TYPE, MimeHelper.TEXT_PLAIN)
                   .direct(HttpResponseStatus.OK, "User-agent: *\n" + "Disallow:\n");
            }
        } else if ("/reset".equals(ctx.getRequestedURI())) {
            ctx.clearSession();
            ctx.respondWith().redirectTemporarily(ctx.get("path").asString("/"));
        } else {
            // Bind user to request if present for translations etc. to work correctly...
            UserContext.getCurrentUser();
            ctx.respondWith()
               .error(HttpResponseStatus.NOT_FOUND,
                      Strings.apply("No dispatcher found for: %s", ctx.getRequestedURI()));
        }

        return DispatchDecision.DONE;
    }
}
