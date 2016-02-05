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
import sirius.kernel.di.std.Context;
import sirius.kernel.di.std.Part;
import sirius.kernel.di.std.Register;
import sirius.kernel.health.Exceptions;
import sirius.kernel.health.HandledException;
import sirius.kernel.health.MemoryBasedHealthMonitor;
import sirius.kernel.health.metrics.Metric;
import sirius.kernel.health.metrics.MetricState;
import sirius.kernel.health.metrics.Metrics;
import sirius.kernel.nls.NLS;
import sirius.kernel.nls.Translation;
import sirius.web.controller.BasicController;
import sirius.web.controller.Controller;
import sirius.web.controller.Page;
import sirius.web.controller.Routed;
import sirius.web.http.WebContext;
import sirius.web.http.session.ServerSession;
import sirius.web.security.Permission;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * Contains the default admin GUI.
 */
@Register(classes = Controller.class)
public class SystemController extends BasicController {

    public static final String PERMISSION_SYSTEM_CONSOLE = "permission-system-console";

    public static final String PERMISSION_SYSTEM_STATE = "permission-system-state";

    @Routed("/system/console")
    @Permission(PERMISSION_SYSTEM_CONSOLE)
    public void console(WebContext ctx) {
        ctx.respondWith().cached().template("/view/system/console.html");
    }

    @Part
    private Cluster cluster;

    @Part
    private Metrics metrics;

    @Context
    private GlobalContext context;

    @Routed("/system/ok")
    public void ok(WebContext ctx) {
        ctx.respondWith().direct(HttpResponseStatus.OK, "OK");
    }

    @Routed("/system/monitor")
    public void monitorNode(WebContext ctx) {
        ctx.respondWith().direct(HttpResponseStatus.OK, cluster.getNodeState().name());
    }

    @Routed("/system/monitor/details")
    public void monitorNodeDetails(WebContext ctx) {
        if (cluster.getNodeState() != MetricState.GREEN) {
            for (Metric m : metrics.getMetrics()) {
                if (m.getState() != MetricState.GREEN && m.getState() != MetricState.GRAY) {
                    ctx.respondWith().direct(HttpResponseStatus.OK, m.getName() + ": " + m.getValueAsString());
                    return;
                }
            }
        }
        ctx.respondWith().direct(HttpResponseStatus.OK, cluster.getNodeState().name());
    }

    @Routed("/system/metric/:1")
    public void metric(WebContext ctx, String key) {
        if (cluster.getNodeState() != MetricState.GREEN) {
            for (Metric m : metrics.getMetrics()) {
                if (Strings.areEqual(key, m.getName())) {
                    ctx.respondWith().direct(HttpResponseStatus.OK, NLS.toMachineString(m.getValue()));
                    return;
                }
            }
        }
        ctx.respondWith().direct(HttpResponseStatus.OK, NLS.toMachineString(0d));
    }

    @Routed("/system/monitor/cluster")
    public void monitorCluster(WebContext ctx) {
        ctx.respondWith().direct(HttpResponseStatus.OK, cluster.getClusterState().name());
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

    @Routed("/system/info")
    public void info(WebContext ctx) {
        ctx.respondWith().template("/view/system/info.html");
    }

    @Routed("/system/reset")
    public void reset(WebContext ctx) {
        ctx.clearSession();
        ctx.getServerSession(false).ifPresent(ServerSession::invalidate);
        ctx.deleteCookie("user");
        ctx.deleteCookie("token");
        ctx.respondWith().direct(HttpResponseStatus.OK, "Session has been cleared...");
    }

    @Routed("/system/state")
    @Permission(PERMISSION_SYSTEM_STATE)
    public void state(WebContext ctx) {
        ctx.respondWith().template("/view/system/state.html", cluster, metrics, ctx.get("all").asBoolean(false));
    }
}
