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
 * If no other dispatcher jumps in, this will take care of handing the request by sending an HTTP/404.
 */
@Register
public class DefaultDispatcher implements WebDispatcher {

    /**
     * Contains the original URI, in case this dispatcher decided to restart the pipeline...
     */
    public static final String ATTRIBUTE_ORIGINAL_URI = "ORIGINAL_URI";

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
        if ("/robots.txt".equals(ctx.getRequestedURI()) && serveRobots) {
            if (robotsDisallowAll) {
                ctx.respondWith()
                   .infinitelyCached()
                   .setHeader(HttpHeaderNames.CONTENT_TYPE, MimeHelper.TEXT_PLAIN)
                   .direct(HttpResponseStatus.OK, """
                           User-agent: *
                           Disallow: /
                           """);
            } else {
                ctx.respondWith()
                   .infinitelyCached()
                   .setHeader(HttpHeaderNames.CONTENT_TYPE, MimeHelper.TEXT_PLAIN)
                   .direct(HttpResponseStatus.OK, """
                           User-agent: *
                           Disallow:
                           """);
            }
        } else if ("/".equals(ctx.getRequestedURI())) {
            // If there is no controller and no other dispatcher which is willing to handle a "/" request, we
            // re-start the pipeline with "/admin". This will at least be picked up by the DashboardController
            // of sirius-biz, which is probably what we want. If this isn't available, we end up with a 404
            // either way...
            //
            // We previously registered the DashboardController directly for "/" with a very high priority number
            // (to put it at the end of the pipeline). However, this prohibits that another dispatcher which runs
            // after the ControllerDispatcher handles "/". Using this approach, either a controller or a dispatch
            // can handle "/" if the DashboardController wanted for this.
            ctx.withCustomURI("/admin");
            return DispatchDecision.RESTART;
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
