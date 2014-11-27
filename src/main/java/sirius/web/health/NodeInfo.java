/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.health;

import com.google.common.collect.Lists;

import java.time.LocalDateTime;
import java.util.List;

/**
 * Contains the state of a cluster member.
 * <p>
 * Will be obtained through {@link Cluster} by calling the members {@link NodeInfoService}.
 * </p>
 *
 * @author Andreas Haufler (aha@scireum.de)
 * @since 2014/01
 */
public class NodeInfo {

    private String name;
    private int priority;
    private String endpoint;
    private LocalDateTime lastPing;
    private int pingFailures;
    private String uptime;
    private MetricState nodeState;
    private MetricState clusterState;
    private List<Metric> metrics = Lists.newArrayList();

    /**
     * Returns the name of the node.
     *
     * @return the name of the node
     */
    public String getName() {
        return name;
    }

    /**
     * Sets the name of the node
     *
     * @param name the name to set
     */
    protected void setName(String name) {
        this.name = name;
    }

    /**
     * Returns the priority of the node within the cluster (lower is better).
     *
     * @return the priority of the node
     */
    public int getPriority() {
        return priority;
    }

    /**
     * Sets the priority of the node.
     *
     * @param priority the priority to set
     */
    protected void setPriority(int priority) {
        this.priority = priority;
    }

    /**
     * Returns the endpoint of the node.
     *
     * @return the endpoint (HTTP address) of the node
     */
    public String getEndpoint() {
        return endpoint;
    }

    /**
     * Sets the endpoint of the node.
     *
     * @param endpoint the endpoint to set. Should be something like "http://host:port"
     */
    protected void setEndpoint(String endpoint) {
        this.endpoint = endpoint;
    }

    /**
     * Returns the timestamp of the last successful call to obtain the node state.
     *
     * @return the timestamp of the last successful communication
     */
    public LocalDateTime getLastPing() {
        return lastPing;
    }

    /**
     * Resets the ping failure counter and sets the lastPing value to <tt>now</tt>
     */
    protected void pingSucceeded() {
        this.lastPing = LocalDateTime.now();
        this.pingFailures = 0;
    }

    /**
     * Returns the state of the node.
     *
     * @return the state of the node
     */
    public MetricState getNodeState() {
        return nodeState;
    }

    /**
     * Sets the state of the node.
     *
     * @param nodeState the new node state
     */
    protected void setNodeState(MetricState nodeState) {
        this.nodeState = nodeState;
    }

    /**
     * Returns the state of the cluster as seen by this node.
     *
     * @return the state of the cluster as seen by this node
     */
    public MetricState getClusterState() {
        return clusterState;
    }

    /**
     * Sets the state of the cluster as seen by this node.
     *
     * @param clusterState the new cluster state
     */
    protected void setClusterState(MetricState clusterState) {
        this.clusterState = clusterState;
    }

    /**
     * Returns all metrics known for this node.
     *
     * @return a list of all known metrics of the node
     */
    public List<Metric> getMetrics() {
        return metrics;
    }

    /**
     * Returns the number of failed attempts to connect to the node.
     *
     * @return the number of failed connect attempts
     */
    public int getPingFailures() {
        return pingFailures;
    }

    /**
     * Increment the ping failure counter.
     */
    public void incPingFailures() {
        this.pingFailures++;
    }

    /**
     * Returns the uptime of the node as a string.
     *
     * @return the uptime of the node
     */
    public String getUptime() {
        return uptime;
    }

    /**
     * Sets the uptime of the node.
     *
     * @param uptime the uptime to set
     */
    protected void setUptime(String uptime) {
        this.uptime = uptime;
    }
}
