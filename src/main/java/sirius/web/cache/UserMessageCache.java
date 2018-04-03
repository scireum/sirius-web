/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.cache;

import sirius.web.controller.Message;

import java.util.List;

/**
 * A cache in which a value for a key can be read once and then is removed again.
 */
public interface UserMessageCache {

    /**
     * Puts a value inside the cache identified by the key.
     *
     * @param key      the key identifying the value
     * @param messages the messages to put inside the cache
     */
    void put(String key, List<Message> messages);

    /**
     * Gets the value identified by the given key and removes it from the cache.
     *
     * @param key the key identifying the value
     * @return the value identified by the key
     */
    List<Message> getAndRemove(String key);
}
