/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.resources;

import sirius.kernel.di.std.Register;

import javax.annotation.Nonnull;
import java.net.URL;

/**
 * Simple resolver which tries to find the given resource in the classpath.
 */
@Register
public class ClasspathResolver implements Resolver {
    private static final String FORCE_ORIGINAL_PREFIX = "/original:";

    @Override
    public Resource resolve(@Nonnull String scopeId, @Nonnull String resource) {
        String resourceToResolve = resource.startsWith(FORCE_ORIGINAL_PREFIX) ? resource.replace(FORCE_ORIGINAL_PREFIX, "/") : resource;

        URL url = getClass().getResource(resourceToResolve);
        if (url != null) {
            return Resource.constantResource(scopeId, resource, url);
        }
        url = getClass().getResource("/default" + resourceToResolve);
        if (url != null) {
            return Resource.constantResource(scopeId, resource, url);
        }
        return null;
    }

    @Override
    public int getPriority() {
        return DEFAULT_PRIORITY;
    }
}
