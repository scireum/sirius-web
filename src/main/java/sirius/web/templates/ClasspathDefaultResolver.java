/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates;

import sirius.kernel.di.std.Register;

import java.net.URL;

/**
 * Default resolver which tries to find the given resource in the "default" sub folder in the classpath.
 * <p>
 * This is used by SIRIUS to deliver default or fallback template which then can be overridden by product specific
 * templates. Therefore this resolver has the lowest priority (999).
 * </p>
 *
 * @author Andreas Haufler (aha@scireum.de)
 * @since 2014/07
 */
@Register
public class ClasspathDefaultResolver implements Resolver {

    @Override
    public Resource resolve(String scopeId, String resource) {
        URL url = getClass().getResource("/default" + (resource.startsWith("/") ? resource : "/" + resource));
        if (url == null) {
            return null;
        }
        return Resource.constantResource(scopeId, resource, url);
    }

    @Override
    public int getPriority() {
        return 999;
    }
}
