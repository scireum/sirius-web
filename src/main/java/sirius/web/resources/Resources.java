/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.resources;

import sirius.kernel.Sirius;
import sirius.kernel.cache.Cache;
import sirius.kernel.cache.CacheManager;
import sirius.kernel.commons.Explain;
import sirius.kernel.di.std.PriorityParts;
import sirius.kernel.di.std.Register;
import sirius.kernel.health.Log;
import sirius.web.security.UserContext;

import javax.annotation.Nonnull;
import java.util.Collection;
import java.util.Objects;
import java.util.Optional;

/**
 * Resolves template names into URLs.
 * <p>
 * Resolves templates by iterating over all known {@link Resolver} implementations while providing a cache for
 * frequently checked names / URIs.
 */
@Register(classes = Resources.class)
public class Resources {

    /**
     * Logger used by the resource discovery framework
     */
    public static final Log LOG = Log.get("resources");

    @PriorityParts(Resolver.class)
    private Collection<Resolver> resolvers;

    /**
     * Cache used to map a scope name and local uri to an URL pointing to a resolved content.
     */
    private Cache<String, Optional<Resource>> resolverCache = CacheManager.createLocalCache("resolver-cache");

    /**
     * Tries to resolve a template or content-file into a {@link Resource}
     *
     * @param uri the local name of the uri to load
     * @return a {@link Resource} (wrapped as resource) pointing to the requested content
     * or an empty optional if no resource was found
     */
    @Nonnull
    public Optional<Resource> resolve(@Nonnull String uri) {
        return resolve(UserContext.getCurrentScope().getScopeId(), uri);
    }

    /**
     * Tries to resolve a template or content-file into a {@link Resource}
     *
     * @param scopeId the scope to use. Use {@link #resolve(String)} to pick the currently active scope
     * @param uri     the local name of the uri to load
     * @return a {@link Resource} (wrapped as resource) pointing to the requested content
     * or an empty optional if no resource was found
     */
    @Nonnull
    @SuppressWarnings("squid:S2789")
    @Explain("We cache optionals and therefore need this null check.")
    public Optional<Resource> resolve(@Nonnull String scopeId, @Nonnull String uri) {
        String effectiveUri = uri.startsWith("/") ? uri : "/" + uri;

        String lookupKey = scopeId + "://" + effectiveUri;
        Optional<Resource> result = resolverCache.get(lookupKey);
        if (result != null) {
            if (Sirius.isDev()) {
                // In dev environments, we always perform a lookup in case something changed
                Optional<Resource> currentResult = resolveURI(scopeId, effectiveUri);
                if (!result.isPresent()) {
                    return currentResult;
                }
                if (!currentResult.isPresent() || !Objects.equals(result.get().getUrl(),
                                                                  currentResult.get().getUrl())) {
                    return currentResult;
                }
            }
            return result;
        }
        result = resolveURI(scopeId, effectiveUri);
        resolverCache.put(lookupKey, result);
        return result;
    }

    /**
     * Deletes the cached resource for the given scope and uri.
     * <p>
     * If one knows that an underlying template was created or deleted and therefore the cached lookup URL has to be
     * removed, this method can be invoked to remove the entry from the cache.
     *
     * @param scopeId the scope to use
     * @param uri     the local name of the uri to flush
     */
    public void flushCache(@Nonnull String scopeId, @Nonnull String uri) {
        String effectiveUri = uri.startsWith("/") ? uri : "/" + uri;

        String lookupKey = scopeId + "://" + effectiveUri;
        resolverCache.remove(lookupKey);
    }

    /*
     * Calls all available resolvers to pick the right content for the given scope and uri (without using a cache)
     */
    private Optional<Resource> resolveURI(String scopeId, String uri) {
        for (Resolver res : resolvers) {
            Resource r = res.resolve(scopeId, uri);
            if (r != null) {
                return Optional.of(r);
            }
        }
        return Optional.empty();
    }
}
