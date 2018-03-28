/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.cache;

import sirius.kernel.cache.Cache;
import sirius.kernel.cache.ValueComputer;
import sirius.kernel.cache.ValueVerifier;

/**
 * Responsible for creating a distributed {@link ReadOnceCache}s.
 */
public interface DistributedReadOnceCacheFactory {

    /**
     * Creates a distributed {@link ReadOnceCache}.
     *
     * @param name        The cache name.
     * @param valueParser Responsible for parsing the value from and to JSON.
     * @param <V>         The type of the cached values.
     * @return the created cache
     */
    <V> ReadOnceCache<V> createDistributedCache(String name, ValueParser<V> valueParser);

    /**
     * Returns whether the DistributedReadOnceCacheFactory is configured and can create caches.
     *
     * @return <tt>true</tt> if DistributedReadOnceCacheFactory, <tt>false</tt> otherwise
     */
    boolean isConfigured();
}
