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
import sirius.web.controller.Message;

import java.util.List;

/**
 * A {@link UserMessageCache} backed by a {@link Cache} created by the {@link CacheManager}.
 */
public class LocalUserMessageCache implements UserMessageCache {

    private Cache<String, List<Message>> cache;

    public LocalUserMessageCache(String name) {
        cache = CacheManager.createCache(name);
    }

    @Override
    public void put(String key, List<Message> value) {
        cache.put(key, value);
    }

    @Override
    public List<Message> getAndRemove(String key) {
        List<Message> value = cache.get(key);
        cache.remove(key);
        return value;
    }
}
