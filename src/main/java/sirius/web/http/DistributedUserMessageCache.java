/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.http;

import sirius.web.controller.Message;

import java.util.List;

/**
 * A cache in which a value for a key can be read once and then is removed again.
 */
public interface DistributedUserMessageCache {

    /**
     * Stores a list of messages for the given identifier.
     *
     * @param key      the key identifying the user to store the messages for
     * @param messages the messages to put inside the cache
     */
    void put(String key, List<Message> messages);

    /**
     * Obtains and removes the messages stored for the given identifier
     *
     * @param key the key identifying the value
     * @return the value identified by the key
     */
    List<Message> getAndRemove(String key);

    /**
     * Determines if the distributed cache layer is available or not.
     *
     * @return <tt>true</tt> if the cache is configured and ready, <tt>false</tt> otherwise
     */
    boolean isReady();
}
