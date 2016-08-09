/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.health;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import com.google.common.base.Charsets;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.google.common.io.CharStreams;
import sirius.kernel.Lifecycle;
import sirius.kernel.Sirius;
import sirius.kernel.async.CallContext;
import sirius.kernel.async.Tasks;
import sirius.kernel.commons.Context;
import sirius.kernel.commons.Strings;
import sirius.kernel.di.std.ConfigValue;
import sirius.kernel.di.std.Part;
import sirius.kernel.di.std.Register;
import sirius.kernel.health.Exceptions;
import sirius.kernel.health.Log;
import sirius.kernel.health.metrics.Metric;
import sirius.kernel.health.metrics.MetricState;
import sirius.kernel.health.metrics.Metrics;
import sirius.kernel.info.Module;
import sirius.kernel.info.Product;
import sirius.kernel.timer.EveryMinute;
import sirius.web.http.WebServer;
import sirius.web.mails.Mails;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;
import java.net.URLConnection;
import java.util.Iterator;
import java.util.LinkedHashMap;
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
@Register(classes = {Cluster.class, EveryMinute.class, Lifecycle.class})
public class Cluster implements EveryMinute, Lifecycle {

    /*
     * Logger used by the cluster system
     */
    public static final Log LOG = Log.get("cluster");

    /*
     * Contains the state of the local node
     */
    private MetricState nodeState = MetricState.GRAY;

    /*
     * Contains the overall state of the cluster (which is equal to the "worst" node state among all members).
     */
    private MetricState clusterState = MetricState.GRAY;

    /*
     * Used to lower message levels for ongoing failures
     */
    private boolean currentlyNotifying = false;

    /*
     * Contains a list of all cluster members.
     */
    private List<NodeInfo> nodes = null;

    @ConfigValue("health.cluster.priority")
    private int priority;

    @ConfigValue("health.cluster.logState")
    private boolean logState;

    @ConfigValue("health.cluster.alerts.mail")
    private List<String> alertReceivers;

    @Part
    private Metrics metrics;

    @Part
    private HipChat hipChat;

    @Part
    private Slack slack;

    /**
     * Reports infos for all known cluster members.
     * <p>
     * During startup (before the initial communication took place) some information (like the node name) might
     * be missing.
     * </p>
     *
     * @return a list of all cluster members along with their last known state
     */
    public List<NodeInfo> getNodeInfos() {
        if (nodes == null) {
            List<NodeInfo> result = Lists.newArrayList();
            for (String endpoint : Sirius.getConfig().getStringList("health.cluster.nodes")) {
                NodeInfo info = new NodeInfo();
                info.setEndpoint(endpoint);
                result.add(info);
            }
            nodes = result;
        }

        return nodes;
    }

    /**
     * Returns the best node which is still functional and has the highest priority (lowest number).
     *
     * @return all known data about the best functional cluster node
     */
    public NodeInfo getBestAvailableNode() {
        for (NodeInfo info : nodes) {
            if (info.getPriority() < priority && info.getNodeState() == MetricState.GREEN) {
                return info;
            }
        }

        return null;
    }

    /**
     * Determines if the current node is the best (highest priority, still functional) cluster node.
     *
     * @return <tt>true</tt> if the current node is the best node in this cluster
     */
    public boolean isBestAvailableNode() {
        for (NodeInfo info : nodes) {
            if ((info.getPriority() < priority
                 || info.getPriority() == priority && info.getName().compareTo(CallContext.getNodeName()) < 0)
                && info.getNodeState() != MetricState.RED) {
                return false;
            }
        }

        return true;
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

    @Part
    private Tasks tasks;

    /*
     * Re-computes the node and cluster state...
     */
    @Override
    public void runTimer() throws Exception {
        tasks.defaultExecutor().fork(this::updateClusterState);
    }

    private void updateClusterState() {
        MetricState newNodeState = computeNodeState();
        MetricState newClusterState = computeClusterState(newNodeState);
        cleanNodeInfos();
        checkClusterState(newClusterState);
        clusterState = newClusterState;
    }

    private void checkClusterState(MetricState newClusterState) {
        if (clusterState == MetricState.RED && newClusterState == MetricState.RED) {
            LOG.FINE("Cluster was RED and remained RED - ensuring alert...");
            if (inCharge(MetricState.RED)) {
                LOG.FINE("This node is in charge of action at the bell....fire alert!");
                alertClusterFailure(!currentlyNotifying);
            }
            currentlyNotifying = true;
        } else if (clusterState == MetricState.RED && newClusterState != MetricState.RED) {
            if (inCharge(newClusterState)) {
                LOG.FINE("Cluster recovered");
                hipChat.sendMessage("cluster",
                                    "Cluster is now in state: " + newClusterState,
                                    HipChat.Color.GREEN,
                                    true);
                slack.sendMessage("cluster", "Cluster is now in state: " + newClusterState, Slack.Color.GOOD);
            }
            currentlyNotifying = false;
        }
        LOG.FINE("Cluster check complete. Status was %s and is now %s", clusterState, newClusterState);
    }

    private void cleanNodeInfos() {
        // Since the cluster.nodes array might contain all nodes of the cluster, we filter out or own (by name)
        Iterator<NodeInfo> iter = getNodeInfos().iterator();
        while (iter.hasNext()) {
            if (Strings.areEqual(CallContext.getNodeName(), iter.next().getName())) {
                iter.remove();
            }
        }
    }

    private MetricState computeClusterState(MetricState newNodeState) {
        MetricState newClusterState = newNodeState;
        LOG.FINE("Scanning cluster...");
        for (NodeInfo info : getNodeInfos()) {
            newClusterState = updateNodeState(newClusterState, info);
        }
        return newClusterState;
    }

    private MetricState computeNodeState() {
        MetricState newNodeState = MetricState.GREEN;
        for (Metric m : metrics.getMetrics()) {
            if (m.getState().ordinal() > newNodeState.ordinal()) {
                newNodeState = m.getState();
            }
            if (m.getState().ordinal() > MetricState.GREEN.ordinal()) {
                String message = Strings.apply("%s is %s (%s)", m.getName(), m.getValueAsString(), m.getState());
                hipChat.sendMessage("metric",
                                    message,
                                    m.getState() == MetricState.YELLOW ? HipChat.Color.YELLOW : HipChat.Color.RED,
                                    false);
                slack.sendMessage("metric",
                                  message,
                                  m.getState() == MetricState.YELLOW ? Slack.Color.WARNING : Slack.Color.DANGER);

                if (logState) {
                    LOG.WARN("NodeState: Metric %s", message);
                }
            }
        }
        this.nodeState = newNodeState;
        return newNodeState;
    }

    private MetricState updateNodeState(MetricState newClusterState, NodeInfo info) {
        try {
            LOG.FINE("Testing node: %s", info.getEndpoint());
            URLConnection c = new URL(info.getEndpoint() + "/service/json/system/node-info").openConnection();
            c.setConnectTimeout(10000);
            c.setReadTimeout(10000);
            c.setDoInput(true);
            c.setDoOutput(false);
            try (InputStream in = c.getInputStream()) {
                JSONObject response = JSON.parseObject(CharStreams.toString(new InputStreamReader(in, Charsets.UTF_8)));
                info.setName(response.getString("name"));
                info.setNodeState(MetricState.valueOf(response.getString("nodeState")));
                if (info.getNodeState().ordinal() > newClusterState.ordinal()) {
                    newClusterState = info.getNodeState();
                }
                info.setClusterState(MetricState.valueOf(response.getString("clusterState")));
                info.setPriority(response.getInteger("priority"));
                info.setUptime(response.getString("uptime"));
                info.getMetrics().clear();
                JSONArray metrics = response.getJSONArray("metrics");
                for (int i = 0; i < metrics.size(); i++) {
                    try {
                        JSONObject metric = (JSONObject) metrics.get(i);
                        Metric m = new Metric(metric.getString("name"),
                                              metric.getDoubleValue("value"),
                                              MetricState.valueOf(metric.getString("state")),
                                              metric.getString("unit"));
                        info.getMetrics().add(m);
                    } catch (Throwable e) {
                        // Ignore non-well-formed metrics...
                        LOG.FINE(e);
                    }
                }
                info.pingSucceeded();
                LOG.FINE("Node: %s is %s (%s)", info.getName(), info.getNodeState(), info.getClusterState());
            }
        } catch (Throwable t) {
            if (t instanceof IOException) {
                if (logState) {
                    LOG.WARN("Cannot reach node %s: %s (%s)",
                             info.getEndpoint(),
                             t.getMessage(),
                             t.getClass().getSimpleName());
                }
            } else {
                Exceptions.handle(LOG, t);
            }
            info.setNodeState(MetricState.RED);
            info.setClusterState(MetricState.RED);
            newClusterState = MetricState.RED;
            info.incPingFailures();
        }
        return newClusterState;
    }

    /*
     * Determines if this node is in charge of sending alerts
     */
    private boolean inCharge(MetricState clusterStateToBroadcast) {
        if (isBestAvailableNode()) {
            return true;
        }
        for (NodeInfo info : getNodeInfos()) {
            if (isBetter(info)
                && info.getClusterState() == clusterStateToBroadcast
                && info.getNodeState() != MetricState.RED) {
                // Another node took care of it...
                LOG.FINE("Node %s is in charge of sending an alert", info.getName());
                return false;
            }
        }
        return true;
    }

    /*
     * Determines if the given node has a better priority than the current node
     */
    private boolean isBetter(NodeInfo info) {
        if (info.getPriority() > getNodePriority()) {
            return false;
        }
        if (info.getPriority() < getNodePriority()) {
            return true;
        }
        return info.getName().compareTo(CallContext.getNodeName()) < 0;
    }

    @Part
    private Mails ms;

    private void alertClusterFailure(boolean firstAlert) {
        if (firstAlert) {
            Context ctx = Context.create()
                                 .set("app", Product.getProduct().toString())
                                 .set("node", CallContext.getNodeName())
                                 .set("nodeState", nodeState.name())
                                 .set("clusterState", clusterState.name())
                                 .set("metrics", metrics)
                                 .set("nodes", nodes);
            for (String receiver : alertReceivers) {
                ms.createEmail().useMailTemplate("system-alert", ctx).toEmail(receiver).send();
            }
            hipChat.sendMessage("cluster", "Cluster is RED", HipChat.Color.RED, firstAlert);
            slack.sendMessage("cluster", "Cluster is RED", Slack.Color.DANGER);
        }

        if (logState) {
            LOG.WARN("NodeState: %s, ClusterState: %s", nodeState, clusterState);
        }
    }

    /**
     * Returns the priority of this node.
     *
     * @return the priority of this node (lower is better)
     */
    public int getNodePriority() {
        return priority;
    }

    @Override
    public int getPriority() {
        return WebServer.LIFECYCLE_PRIORITY + 100;
    }

    @Override
    public void started() {
        hipChat.sendMessage("start", "Node is starting up...", HipChat.Color.GREEN, false);
        LinkedHashMap<String, String> ctx = Maps.newLinkedHashMap();
        ctx.put("Product", Product.getProduct().getDetails());
        for (Module m : Product.getModules()) {
            ctx.put(m.getName(), m.getDetails());
        }
        slack.sendMessage("start", "Node is starting up...", Slack.Color.GOOD, ctx);
    }

    @Override
    public void stopped() {
        hipChat.sendMessage("start", "Node is starting up...", HipChat.Color.GRAY, true);
        slack.sendMessage("stop", "Node is shutting down...", Slack.Color.WARNING);
    }

    @Override
    public void awaitTermination() {
    }

    @Override
    public String getName() {
        return "Cluster";
    }
}
