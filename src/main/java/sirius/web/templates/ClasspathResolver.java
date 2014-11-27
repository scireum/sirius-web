/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates;

import sirius.kernel.di.std.Priorized;
import sirius.kernel.di.std.Register;

import java.net.URL;

/**
 * Simple resolver which tries to find the given resource in the classpath.
 *
 * @author Andreas Haufler (aha@scireum.de)
 * @since 2014/07
 */
@Register
public class ClasspathResolver implements Resolver {

    @Override
    public Resource resolve(String scopeId, String resource) {
        URL url = getClass().getResource(resource.startsWith("/") ? resource : "/" + resource);
        if (url == null) {
            return null;
        }
        return Resource.constantResource(scopeId, resource, url);
    }

    @Override
    public int getPriority() {
        return Priorized.DEFAULT_PRIORITY;
    }
}
