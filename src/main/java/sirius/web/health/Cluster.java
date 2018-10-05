/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.health;

import sirius.kernel.async.Tasks;
import sirius.kernel.di.std.Part;
import sirius.kernel.di.std.Register;
import sirius.kernel.health.Log;
import sirius.kernel.health.metrics.Metric;
import sirius.kernel.health.metrics.MetricState;
import sirius.kernel.health.metrics.Metrics;
import sirius.kernel.timer.EveryMinute;

import java.util.Collections;
import java.util.List;

/**
 * Manages and monitors the state of a cluster of machines.
 * <p>
 * Permits to couple a number of machines to a cluster where each member monitors the others. In case or a failure
 * an alert will be triggered. Additionally the cluster state can be visualized using the web interface
 * (/system/state).
 * <p>
 * Even in a single machine installation, this class will take care of monitoring all metrics and triggering an
 * alert (if possible).
 * <p>
 * Cluster members are defined via the configuration by listing all HTTP-Endpoints under
 * <tt>health.cluster.nodes</tt> in the form of <tt>http://hostname:port</tt>. Each node always defines a priority
 * (<tt>health.cluster.priority</tt>). The node with the lowest number (which is still functional) is in charge
 * of triggering an alert in case of faulting or unreachable members.
 */
@Register(classes = {Cluster.class, EveryMinute.class})
public class Cluster implements EveryMinute {

    /**
     * Logger used by the cluster system
     */
    public static final Log LOG = Log.get("cluster");

    /**
     * Contains the state of the local node
     */
    private MetricState nodeState = MetricState.GRAY;

    /**
     * Contains the overall state of the cluster (which is equal to the "worst" node state among all members).
     */
    private MetricState clusterState = MetricState.GRAY;

    /**
     * Determines if an alarm is currently present
     */
    private boolean alarmPresent = false;

    /**
     * Contains a list of all cluster members.
     */
    private List<NodeInfo> nodes = Collections.emptyList();

    @Part
    private Tasks tasks;

    @Part
    private Metrics metrics;

    @Part
    private ClusterManager manager;

    /**
     * Reports infos for all known cluster members.
     * <p>
     * During startup (before the initial communication took place) some information (like the node name) might
     * be missing.
     *
     * @return a list of all cluster members along with their last known state
     */
    public List<NodeInfo> getNodeInfos() {
        return Collections.unmodifiableList(nodes);
    }

    /**
     * Returns the determined state of this node.
     *
     * @return the state of this node, which is computed every minute
     */
    public MetricState getNodeState() {
        return nodeState;
    }

    /**
     * Returns the overall state of the cluster.
     *
     * @return the cluster state, which is the "worst" state among all members
     */
    public MetricState getClusterState() {
        return clusterState;
    }

    /**
     * Determines if an alarm is currently present.
     *
     * @return <tt>true</tt> if the cluster is in an invalid state, <tt>false</tt> otherwise
     */
    public boolean isAlarmPresent() {
        return alarmPresent;
    }

    @Override
    public void runTimer() throws Exception {
        tasks.defaultExecutor().fork(this::updateClusterState);
    }

    private void updateClusterState() {
        if (manager != null) {
            nodes = manager.updateClusterState();
        }

        nodeState = computeNodeState();

        MetricState lastClusterState = clusterState;
        clusterState = getNodeInfos().stream()
                                     .map(NodeInfo::getNodeState)
                                     .reduce(nodeState, (a, b) -> a.ordinal() > b.ordinal() ? a : b);

        alarmPresent = lastClusterState == MetricState.RED && clusterState == MetricState.RED;
    }

    private MetricState computeNodeState() {
        MetricState newNodeState = MetricState.GREEN;
        for (Metric m : metrics.getMetrics()) {
            if (m.getState().ordinal() > newNodeState.ordinal()) {
                newNodeState = m.getState();
            }
            if (nodeState != MetricState.RED && m.getState() == MetricState.RED) {
                LOG.WARN("NodeState: Metric %s is %s", m.getLabel(), m.getValueAsString());
            }
        }

        return newNodeState;
    }
}
