/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.health;

import java.util.List;

/**
 * Provides a list of all known members of a cluster.
 */
public interface ClusterManager {

    /**
     * Invoked in regular intervals to determine the list of available nodes and their state.
     *
     * @return a list of known nodes and their state
     */
    List<NodeInfo> updateClusterState();
}
