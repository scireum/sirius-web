/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.cache;

import sirius.kernel.di.GlobalContext;
import sirius.kernel.di.std.Part;
import sirius.kernel.health.Log;

/**
 * Responsible for creating {@link ReadOnceCache}s.
 */
public class ReadOnceCacheManager {

    @Part
    private static GlobalContext ctx;

    public static final Log LOG = Log.get("cache");

    private ReadOnceCacheManager() {

    }

    /**
     * Creates a {@link LocalReadOnceCache}, considering the config <tt>cache.[name].ttl</tt> for the cache.
     *
     * @param name The cache name.
     * @return The created cache.
     */
    public static ReadOnceCache createLocalReadOnceCache(String name) {
        return new LocalReadOnceCache(name);
    }

    /**
     * Creates a distributed {@link ReadOnceCache} if a {@link DistributedReadOnceCacheFactory} is implemented and can be
     * injected. A {@link LocalReadOnceCache} otherwise.
     * <p>
     * Considers the config <tt>cache.[name].ttl</tt> for the cache.
     *
     * @param name The cache name.
     * @return The created cache.
     */
    public static ReadOnceCache createDistributedReadOnceCache(String name) {
        DistributedReadOnceCacheFactory distributedCacheFactory = ctx.getPart(DistributedReadOnceCacheFactory.class);
        if (distributedCacheFactory == null || !distributedCacheFactory.isConfigured()) {
            LOG.WARN("No DistributedReadOnceCacheFactory is found or ready (yet)! Creating regular cache. "
                     + "DistributedReadOnceCacheFactory are injected at runtime, so maybe you need to wait for system "
                     + "start.");
            return new LocalReadOnceCache(name);
        }
        return distributedCacheFactory.createDistributedCache(name);
    }
}
