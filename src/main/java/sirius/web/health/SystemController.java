/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.health;

import com.google.common.base.Charsets;
import io.netty.handler.codec.http.HttpResponseStatus;
import sirius.kernel.Sirius;
import sirius.kernel.commons.MultiMap;
import sirius.kernel.commons.Strings;
import sirius.kernel.commons.Tuple;
import sirius.kernel.di.GlobalContext;
import sirius.kernel.di.std.Part;
import sirius.kernel.di.std.Register;
import sirius.kernel.health.Exceptions;
import sirius.kernel.health.Microtiming;
import sirius.kernel.health.metrics.Metric;
import sirius.kernel.health.metrics.MetricState;
import sirius.kernel.health.metrics.Metrics;
import sirius.kernel.nls.NLS;
import sirius.web.controller.BasicController;
import sirius.web.controller.Controller;
import sirius.web.controller.Page;
import sirius.web.controller.Routed;
import sirius.web.http.WebContext;
import sirius.web.security.Permission;

import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.util.Collection;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

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
     * Describes the permission required to access the system console.
     */
    public static final String PERMISSION_SYSTEM_TIMING = "permission-system-timing";

    /**
     * Describes the permission required to view the system state.
     */
    public static final String PERMISSION_SYSTEM_STATE = "permission-system-state";

    /**
     * Renders the UI for the system console.
     *
     * @param ctx the request being handled
     */
    @Routed("/system/console")
    @Permission(PERMISSION_SYSTEM_CONSOLE)
    public void console(WebContext ctx) {
        ctx.respondWith().cached().template("templates/system/console.html.pasta");
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
     * Determines if there is currently an ALARM present or not for: <tt>/system/monitor</tt>
     * <p>
     * Either reports OK or ERROR, if the cluster state is RED for at least two intervals (minutes).
     *
     * @param ctx the request being handled
     */
    @Routed("/system/monitor")
    public void monitorNode(WebContext ctx) {
        ctx.respondWith()
           .direct(HttpResponseStatus.OK,
                   cluster.getNodeState() == MetricState.RED && cluster.isAlarmPresent() ? "ERROR" : "OK");
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
            if (Strings.areEqual(key, m.getCode())) {
                ctx.respondWith().direct(HttpResponseStatus.OK, NLS.toMachineString(m.getValue()));
                return;
            }
        }
        ctx.respondWith().direct(HttpResponseStatus.OK, NLS.toMachineString(0d));
    }

    /**
     * Sends all known metrics in a format understood by <b>prometheus.io</b>.
     *
     * @param ctx the request being handled
     */
    @Routed("/system/metrics")
    public void metrics(WebContext ctx) {
        try (PrintWriter out = new PrintWriter(new OutputStreamWriter(ctx.respondWith()
                                                                         .outputStream(HttpResponseStatus.OK,
                                                                                       "text/plain; version=0.0.4"),
                                                                      Charsets.UTF_8))) {
            for (Metric m : metrics.getMetrics()) {
                out.print(m.getCode());
                out.print(" ");
                out.println(NLS.toMachineString(m.getValue()));
            }
        }
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
        ctx.respondWith().template("templates/system/info.html.pasta");
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
        ctx.respondWith()
           .template("templates/system/state.html.pasta",
                     cluster,
                     metrics,
                     ctx.get("all").asBoolean(false),
                     NLS.convertDuration(Sirius.getUptimeInMilliseconds(), true, false));
    }

    /**
     * Provides a list of recorded micro timings.
     *
     * @param ctx the current request
     */
    @Routed("/system/timing")
    @Permission(PERMISSION_SYSTEM_TIMING)
    public void timing(WebContext ctx) {
        if (ctx.hasParameter("enable")) {
            Microtiming.setEnabled(true);
        }
        if (ctx.hasParameter("disable")) {
            Microtiming.setEnabled(false);
        }

        String periodSinceReset = getPeriodSinceLastReset();

        Page<String> page = new Page<>();
        page.bindToRequest(ctx);

        List<Tuple<String, Collection<Tuple<String, String>>>> timings = computeTimingInfos(page);

        ctx.respondWith()
           .template("templates/system/timing.html.pasta", Microtiming.isEnabled(), timings, page, periodSinceReset);
    }

    private List<Tuple<String, Collection<Tuple<String, String>>>> computeTimingInfos(Page<String> page) {
        MultiMap<String, Tuple<String, String>> timingMap = MultiMap.createOrdered();
        String query = Strings.isFilled(page.getQuery()) ? page.getQuery().toLowerCase() : null;
        List<Microtiming.Timing> timings = Microtiming.getTimings();
        timings.sort(Comparator.comparingDouble(t -> t.getAvg().getCount() * t.getAvg().getAvg() * -1d));
        for (Microtiming.Timing timing : timings) {
            if (matchesQuery(query, timing)) {
                timingMap.put(timing.getCategory(),
                              Tuple.create(timing.getKey(),
                                           NLS.toUserString(timing.getAvg().getCount())
                                           + " ("
                                           + NLS.toUserString(timing.getAvg().getAvg() / 1000)
                                           + " ms)"));
            }
        }

        return timingMap.getUnderlyingMap()
                        .entrySet()
                        .stream()
                        .map(e -> Tuple.create(e.getKey(), e.getValue()))
                        .collect(Collectors.toList());
    }

    private boolean matchesQuery(String query, Microtiming.Timing timing) {
        return query == null || timing.getKey().toLowerCase().contains(query) || timing.getCategory()
                                                                                       .toLowerCase()
                                                                                       .contains(query);
    }

    private String getPeriodSinceLastReset() {
        if (Microtiming.isEnabled()) {
            return NLS.convertDuration(System.currentTimeMillis() - Microtiming.getLastReset(), true, true);
        }
        return "";
    }
}
