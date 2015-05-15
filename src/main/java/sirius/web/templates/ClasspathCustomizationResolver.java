/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates;

import sirius.kernel.Sirius;
import sirius.kernel.di.std.Register;

import java.net.URL;

/**
 * Simple resolver which tries to find the given resource in the classpath of the enabled customizations.
 *
 * @author Andreas Haufler (aha@scireum.de)
 * @since 2014/08
 */
@Register
public class ClasspathCustomizationResolver implements Resolver {

    @Override
    public Resource resolve(String scopeId, String resource) {
        URL effectiveUrl = null;
        for (String config : Sirius.getActiveConfigurations()) {
            URL url = getClass().getResource("/customizations/" + config + resource);
            if (url != null) {
                effectiveUrl = url;
            }
        }
        if (effectiveUrl == null) {
            return null;
        }
        return Resource.constantResource(scopeId, resource, effectiveUrl);
    }

    @Override
    public int getPriority() {
        return 90;
    }
}
