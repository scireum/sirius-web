/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.health;

import sirius.kernel.health.metrics.Metric;
import sirius.kernel.health.metrics.MetricState;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Contains the state of a cluster member.
 */
public class NodeInfo {

    private String name;
    private String endpoint;
    private String uptime;
    private MetricState nodeState;
    private List<Metric> metrics = new ArrayList<>();

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
    public void setName(String name) {
        this.name = name;
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
    public void setEndpoint(String endpoint) {
        this.endpoint = endpoint;
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
    public void setNodeState(MetricState nodeState) {
        this.nodeState = nodeState;
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
    public void setUptime(String uptime) {
        this.uptime = uptime;
    }
}
