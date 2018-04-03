/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.cache;

/**
 * A cache in which a value for a key can be read once and then is removed again.
 */
public interface ReadOnceCache {

    /**
     * Puts a value inside the cache identified by the key.
     *
     * @param key   the key identifying the value
     * @param value the value to put inside the cache
     */
    void put(String key, String value);

    /**
     * Gets the value identified by the given key and removes it from the cache.
     *
     * @param key the key identifying the value
     * @return the value identified by the key
     */
    String getAndRemove(String key);
}
