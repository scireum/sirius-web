/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.resources;

import sirius.kernel.di.std.Register;

import java.net.URL;

/**
 * Simple resolver which tries to find the given resource in the classpath.
 */
@Register
public class ClasspathResolver implements Resolver {

    @Override
    public Resource resolve(String scopeId, String resource) {
        URL url = getClass().getResource(resource);
        if (url != null) {
            return Resource.constantResource(scopeId, resource, url);
        }
        url = getClass().getResource("/default" + resource);
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
