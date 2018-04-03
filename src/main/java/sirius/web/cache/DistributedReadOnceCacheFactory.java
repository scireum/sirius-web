/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.cache;

/**
 * Responsible for creating a distributed {@link ReadOnceCache}s.
 */
public interface DistributedReadOnceCacheFactory {

    /**
     * Creates a distributed {@link ReadOnceCache}.
     *
     * @param name        The cache name.
     * @return the created cache
     */
    ReadOnceCache createDistributedCache(String name);

    /**
     * Returns whether the DistributedReadOnceCacheFactory is configured and can create caches.
     *
     * @return <tt>true</tt> if DistributedReadOnceCacheFactory, <tt>false</tt> otherwise
     */
    boolean isConfigured();
}
