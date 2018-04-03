/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.cache;

/**
 * Responsible for creating a distributed {@link UserMessageCache}s.
 */
public interface DistributedUserMessageCacheFactory {

    /**
     * Creates a distributed {@link UserMessageCache}.
     *
     * @param name        The cache name.
     * @return the created cache
     */
    UserMessageCache createDistributedCache(String name);

    /**
     * Returns whether the DistributedUserMessageCacheFactory is configured and can create caches.
     *
     * @return <tt>true</tt> if DistributedUserMessageCacheFactory, <tt>false</tt> otherwise
     */
    boolean isConfigured();
}
