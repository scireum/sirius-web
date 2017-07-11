/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.health;

import io.netty.handler.codec.http.HttpResponseStatus;
import sirius.kernel.commons.Strings;
import sirius.kernel.di.GlobalContext;
import sirius.kernel.di.std.Part;
import sirius.kernel.di.std.Register;
import sirius.kernel.health.Exceptions;
import sirius.kernel.health.metrics.Metric;
import sirius.kernel.health.metrics.MetricState;
import sirius.kernel.health.metrics.Metrics;
import sirius.kernel.nls.NLS;
import sirius.web.controller.BasicController;
import sirius.web.controller.Controller;
import sirius.web.controller.Routed;
import sirius.web.http.WebContext;
import sirius.web.http.session.ServerSession;
import sirius.web.security.Permission;

/**
 * Contains the default admin GUI.
 */
@Register(classes = Controller.class)
public class SystemController extends BasicController {

    @Part
    private Cluster cluster;

    @Part
    private Metrics metrics;

    @Part
    private GlobalContext context;

    /**
     * Describes the permission required to access the system console.
     */
    public static final String PERMISSION_SYSTEM_CONSOLE = "permission-system-console";

    /**
     * Describes the permission required to view the system state.
     */
    public static final String PERMISSION_SYSTEM_STATE = "permission-system-state";

    @Routed("/system/console")
    @Permission(PERMISSION_SYSTEM_CONSOLE)
    public void console(WebContext ctx) {
        ctx.respondWith().cached().template("/templates/system/console.html.pasta");
    }

    /**
     * Simply responds with OK for <tt>/system/ok</tt>
     * <p>
     * This can be used to monitoring tools the check the system health.
     *
     * @param ctx the request being handled
     */
    @Routed("/system/ok")
    public void ok(WebContext ctx) {
        ctx.respondWith().direct(HttpResponseStatus.OK, "OK");
    }

    /**
     * Sends the current node state for <tt>/system/monitor</tt>
     *
     * @param ctx the request being handled
     */
    @Routed("/system/monitor")
    public void monitorNode(WebContext ctx) {
        ctx.respondWith().direct(HttpResponseStatus.OK, cluster.getNodeState() == MetricState.RED ? "ERROR" : "OK");
    }

    /**
     * Sends the current cluster state for <tt>/system/monitor/cluster</tt>
     *
     * @param ctx the request being handled
     */
    @Routed("/system/monitor/cluster")
    public void monitorCluster(WebContext ctx) {
        ctx.respondWith().direct(HttpResponseStatus.OK, cluster.getClusterState() == MetricState.RED ? "ERROR" : "OK");
    }

    /**
     * Sends the value for the requested metric for <tt>/system/metric/[name]</tt>
     *
     * @param ctx the request being handled
     * @param key the name of the metric to fetch
     */
    @Routed("/system/metric/:1")
    public void metric(WebContext ctx, String key) {
        for (Metric m : metrics.getMetrics()) {
            if (Strings.areEqual(key, m.getName())) {
                ctx.respondWith().direct(HttpResponseStatus.OK, NLS.toMachineString(m.getValue()));
                return;
            }
        }
        ctx.respondWith().direct(HttpResponseStatus.OK, NLS.toMachineString(0d));
    }

    /**
     * Can be used to forcefully create an error. (A HandledException in this case.)
     *
     * @param ctx the current request
     */
    @Routed("/system/fail")
    public void fail(WebContext ctx) {
        throw Exceptions.createHandled().withSystemErrorMessage("Forced Exception").handle();
    }

    /**
     * Reports useful information for the current user agent and request.
     * <p>
     * This will output all headers and session information available for the current request
     *
     * @param ctx the current request
     */
    @Routed("/system/info")
    public void info(WebContext ctx) {
        ctx.respondWith().template("/view/system/info.html");
    }

    /**
     * Clears the server and client session.
     * <p>
     * Clears all session data available for the current request.
     *
     * @param ctx the current request
     */
    @Routed("/system/reset")
    public void reset(WebContext ctx) {
        ctx.clearSession();
        ctx.getServerSession(false).ifPresent(ServerSession::invalidate);
        ctx.respondWith().direct(HttpResponseStatus.OK, "Session has been cleared...");
    }

    /**
     * Reports the system and cluster state.
     *
     * @param ctx the current request
     */
    @Routed("/system/state")
    @Permission(PERMISSION_SYSTEM_STATE)
    public void state(WebContext ctx) {
        ctx.respondWith().template("/view/system/state.html", cluster, metrics, ctx.get("all").asBoolean(false));
    }
}
