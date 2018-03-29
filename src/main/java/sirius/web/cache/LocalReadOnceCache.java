/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.cache;

import sirius.kernel.cache.Cache;
import sirius.kernel.cache.CacheManager;

/**
 * A {@link ReadOnceCache} backed by a {@link Cache} created by the {@link CacheManager}.
 *
 * @param <V> The type of the value.
 */
public class LocalReadOnceCache<V> implements ReadOnceCache<V> {

    private Cache<String, V> cache;

    public LocalReadOnceCache(String name) {
        cache = CacheManager.createCache(name);
    }

    @Override
    public void put(String key, V value) {
        cache.put(key, value);
    }

    @Override
    public V getAndRemove(String key) {
        V value = cache.get(key);
        cache.remove(key);
        return value;
    }
}
