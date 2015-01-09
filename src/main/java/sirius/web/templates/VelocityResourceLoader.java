/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates;

import org.apache.commons.collections.ExtendedProperties;
import org.apache.velocity.exception.ResourceNotFoundException;
import org.apache.velocity.runtime.resource.Resource;
import org.apache.velocity.runtime.resource.loader.ResourceLoader;
import sirius.kernel.di.std.Part;
import sirius.kernel.health.Exceptions;

import java.io.InputStream;
import java.net.URL;
import java.net.URLConnection;

/**
 * Adapter to make Velocity use our {@link Resolver} framework.
 * <p>
 * This class needs to be public so it can be instantiated by Velocity.
 *
 * @author Andreas Haufler (aha@scireum.de)
 * @since 2014/02
 */
public class VelocityResourceLoader extends ResourceLoader {

    @Part
    private static Content content;

    public static final VelocityResourceLoader INSTANCE = new VelocityResourceLoader();

    @Override
    public long getLastModified(Resource resource) {
        return getLastModified(resource.getName());
    }

    /**
     * Resolves the given relative path into an URL.
     *
     * @param name the file to resolve
     * @return an URL to the file or <tt>null</tt> if no such file was found
     */
    public URL resolve(String name) {
        if (name == null) {
            return null;
        }
        return content.resolve(name).map(r -> r.getUrl()).orElse(null);
    }

    /**
     * Provides the last change time of the requested resource.
     *
     * @param name name of the resource to check
     * @return the last modification time in milliseconds. If the resource does not exist,
     * <code>System.currentTimeMillis()</code> will be returned
     */
    public long getLastModified(String name) {
        try {
            URL url = resolve(name);
            if (url == null) {
                return System.currentTimeMillis();
            }
            URLConnection c = url.openConnection();
            try {
                // Close the input stream since the stupid implementation of
                // SUN's FileURLConnection always keeps an InputStream open on
                // connect.
                c.getInputStream().close();
            } catch (Throwable e) {
                Content.LOG.WARN(e);
            }
            return c.getLastModified();
        } catch (Throwable e) {
            Exceptions.handle(Content.LOG, e);
            return System.currentTimeMillis();
        }
    }

    /**
     * Determines if the resource exists.
     *
     * @param source name of the requested resource
     * @return <tt>true</tt> if the resource was found, <tt>false</tt> otherwise
     */
    public boolean hasResource(String source) {
        try {
            return resolve(source) != null;
        } catch (Exception e) {
            return false;
        }
    }

    @Override
    public InputStream getResourceStream(String source) throws ResourceNotFoundException {
        try {
            return resolve(source).openStream();
        } catch (Throwable e) {
            throw new ResourceNotFoundException(e);
        }
    }

    @Override
    public void init(ExtendedProperties configuration) {
        // Disable velocity cache! Some artifacts can resolve the same name
        // into different artifacts!
        setCachingOn(false);
        setModificationCheckInterval(1);
    }

    @Override
    public boolean isSourceModified(Resource resource) {
        // Don't support any caching
        return true;
    }

}
