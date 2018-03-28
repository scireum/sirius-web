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
 *
 * @param <V> The type of teh value
 */
public interface ReadOnceCache<V> {

    void put(String key, V value);

    V getAndRemove(String key);
}
