package sirius.web.templates;

import sirius.kernel.di.std.Priorized;

import javax.annotation.Nonnull;

/**
 * Resolves a given resource name to an URL. Implementations can be registered in the component model
 * using {@link sirius.kernel.di.std.Register}.
 * <p>
 * This is used by the {@link Content#resolve(String)} to lookup resources.
 */
public interface Resolver extends Priorized {

    /**
     * Resolves the given resource into an URL.
     *
     * @param scopeId  the id of the currently active scope
     * @param resource the resource to resolve. This will always start with a "/"
     * @return a {@link sirius.web.templates.Resource} pointing to the data to use or <tt>null</tt> if this resolver
     * cannot resolve the given resource
     */
    Resource resolve(@Nonnull String scopeId, @Nonnull String resource);
}
