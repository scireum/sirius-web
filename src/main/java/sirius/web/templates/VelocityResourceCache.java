/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates;

import org.apache.velocity.runtime.RuntimeServices;
import org.apache.velocity.runtime.resource.Resource;
import org.apache.velocity.runtime.resource.ResourceCache;

import java.util.Collections;
import java.util.Iterator;

/**
 * Simple implementation of the resource cache as we might use complex lookups which are not reflected in the template
 * name or url.
 * <p>This class needs to be public so it can be instantiated by Velocity.</p>
 *
 * @author Andreas Haufler (aha@scireum.de)
 * @since 2014/02
 */
public class VelocityResourceCache implements ResourceCache {

    @Override
    public void initialize(RuntimeServices rs) {
        // Caching in velocity is disabled, because we resolve the files via the
        // same name into different artifacts...
    }

    @Override
    public Resource get(Object resourceKey) {
        return null;
    }

    @Override
    public Resource put(Object resourceKey, Resource resource) {
        return resource;
    }

    @Override
    public Resource remove(Object resourceKey) {
        return null;
    }

    @Override
    public Iterator<Object> enumerateKeys() {
        return Collections.emptyList().iterator();
    }

}
