/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.health;

import sirius.kernel.cache.CacheManager;
import sirius.kernel.cache.InlineCache;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.function.Consumer;

/**
 * Provides a basic implementation which caches all computed infos for up to ten seconds.
 */
public abstract class CachingLoadInfoProvider implements LoadInfoProvider {

    private final InlineCache<List<LoadInfo>> inlineCache = CacheManager.createTenSecondsInlineCache(this::computeLoadInfos);

    @Override
    public List<LoadInfo> collectLoadInfos() {
        return Collections.unmodifiableList(inlineCache.get());
    }

    protected List<LoadInfo> computeLoadInfos() {
        List<LoadInfo> result = new ArrayList<>();
        computeLoadInfos(result::add);
        return result;
    }

    /**
     * Invoked to compute the actual load infos.
     * <p>
     * There results will be cached up to ten seconds.
     *
     * @param infos the consumer to be supplied with the load infos
     */
    protected abstract void computeLoadInfos(Consumer<LoadInfo> infos);
}
