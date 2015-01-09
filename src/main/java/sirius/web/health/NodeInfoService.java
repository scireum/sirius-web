/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.health;

import sirius.kernel.Sirius;
import sirius.kernel.async.CallContext;
import sirius.kernel.di.std.Part;
import sirius.kernel.di.std.Register;
import sirius.kernel.nls.NLS;
import sirius.kernel.xml.StructuredOutput;
import sirius.web.services.ServiceCall;
import sirius.web.services.StructuredService;

/**
 * Provides statistics for this node.
 * <p>
 * This service will be called by the {@link Cluster} manager running on the other members.
 *
 * @author Andreas Haufler (aha@scireum.de)
 * @since 2014/01
 */
@Register(name = "system/node-info")
public class NodeInfoService implements StructuredService {

    @Part
    private Cluster cluster;

    @Part
    private Metrics metrics;

    @Override
    public void call(ServiceCall call, StructuredOutput out) throws Exception {
        out.beginResult();
        out.property("name", CallContext.getNodeName());
        out.property("nodeState", cluster.getNodeState().toString());
        out.property("clusterState", cluster.getClusterState().toString());
        out.property("uptime", NLS.convertDuration(Sirius.getUptimeInMilliseconds(), true, false));
        out.property("priority", cluster.getNodePriority());
        out.beginArray("metrics");
        for (Metric m : metrics.getMetrics()) {
            out.beginObject("metric");
            out.property("name", m.getName());
            out.property("value", m.getValue());
            out.property("unit", m.getUnit());
            out.property("state", m.getState().name());
            out.endObject();
        }
        out.endArray();
        out.endResult();
    }
}
