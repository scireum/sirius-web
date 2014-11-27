/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates;

import com.google.common.base.Charsets;
import com.google.common.io.ByteStreams;
import sirius.kernel.Sirius;
import sirius.kernel.commons.RateLimit;
import sirius.kernel.commons.Strings;
import sirius.kernel.health.Exceptions;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.net.URLConnection;
import java.util.Objects;
import java.util.concurrent.TimeUnit;

/**
 * Wraps an resolved URL as resource.
 * <p>Used as result of a {@link sirius.web.templates.Resolver}. Next to the resolved URL it also stores the original
 * scope and path which was used to resolve the url.</p>
 *
 * @author Andreas Haufler (aha@scireum.de)
 * @see sirius.web.templates.Resolver
 * @see sirius.web.security.UserContext#getCurrentScope()
 * @since 2014/07
 */
public class Resource {
    private String scopeId;
    private String path;
    private URL url;
    private final boolean constant;
    private long lastModified = -1;
    private boolean consideredConstant;
    private RateLimit checkInterval = RateLimit.timeInterval(10, TimeUnit.SECONDS);
    private long minLastModified;

    /**
     * Creates a new dynamic resource for the given scope, path and resulting url.
     * <p>
     * A dynamic resource might change over time (like a customer specific file on disk). Therefore we
     * frequently check for changes.
     * </p>
     *
     * @param scopeId the scope which was used to resolve the resource
     * @param path    the local path pointing to the resource
     * @param url     the url pointing to the actual content
     * @return a new resource based on the given parameters
     */
    public static Resource dynamicResource(String scopeId, String path, URL url) {
        return new Resource(scopeId, path, url, false);
    }

    /**
     * Creates a new constant resource for the given scope, path and resulting url.
     * <p>
     * A constant resource will not change over time. An example might be a file in the classpath. Note that in
     * debug mode ({@link sirius.kernel.Sirius#isDev()}) all resources are considered dynamic.
     * </p>
     *
     * @param scopeId the scope which was used to resolve the resource
     * @param path    the local path pointing to the resource
     * @param url     the url pointing to the actual content
     * @return a new resource based on the given parameters
     */
    public static Resource constantResource(String scopeId, String path, URL url) {
        return new Resource(scopeId, path, url, true);
    }

    private Resource(String scopeId, String path, URL url, boolean constant) {
        this.scopeId = scopeId;
        this.path = path;
        this.url = url;
        this.constant = constant;
        this.minLastModified = System.currentTimeMillis();

        Objects.requireNonNull(scopeId);
        Objects.requireNonNull(path);
        Objects.requireNonNull(url);
    }

    /**
     * Returns the scope id which was set when resolving the resource.
     *
     * @return the scope id for which the resource was resolved
     */
    public String getScopeId() {
        return scopeId;
    }

    /**
     * Returns the path which was used to lookup the resource.
     *
     * @return the path used to resolve the resource
     */
    public String getPath() {
        return path;
    }

    /**
     * Returns the effective URL pointing to the resolved content
     *
     * @return the URL pointing to the content
     */
    public URL getUrl() {
        return url;
    }

    /**
     * Returns the contents of the resource as {@link java.io.InputStream}.
     *
     * @return an input stream containing the contents of the resource
     */
    public InputStream openStream() {
        try {
            return getUrl().openConnection().getInputStream();
        } catch (IOException e) {
            throw Exceptions.handle(e);
        }
    }

    /**
     * Returns the contents of the resource as byte array.
     *
     * @return the contents of the resource
     */
    public byte[] getContent() {
        try {
            try (InputStream in = openStream()) {
                return ByteStreams.toByteArray(in);
            }
        } catch (IOException e) {
            throw Exceptions.handle(e);
        }
    }

    /**
     * Returns the contents of the resource as string.
     *
     * @return the contents of the resource as string (expecting UTF-8 as encoding).
     */
    public String getContentAsString() {
        return new String(getContent(), Charsets.UTF_8);
    }

    /**
     * Computes the last modified date.
     * <p>
     * This value will be cached if the resource is considered constant. However, in development systems
     * the resource will sill be checked in a regular interval.
     * </p>
     * <p>
     * Also note that the last modified timestamp is
     * at least the timestamp when the resource was resolved. This is required to manage dynamic resources which
     * suddenly are created or vanish.
     * </p>
     *
     * @return the last modified value of the underlying resource.
     */
    public long getLastModified() {
        if (lastModified == -1 || ((!consideredConstant || Sirius.isDev()) && checkInterval.check())) {
            try {
                URLConnection c = url.openConnection();
                try {
                    // Close the input stream since the stupid implementation of
                    // SUN's FileURLConnection always keeps an InputStream open on
                    // connect.
                    c.getInputStream().close();
                } catch (Throwable e) {
                    Content.LOG.WARN(e);
                }
                lastModified = c.getLastModified();
            } catch (IOException e) {
                Exceptions.handle(e);
                lastModified = System.currentTimeMillis();
            }
        }
        return Math.max(lastModified, minLastModified);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == this) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (!(obj instanceof Resource)) {
            return false;
        }

        return Objects.equals(path, ((Resource) obj).path) && Objects.equals(scopeId, ((Resource) obj).scopeId);
    }

    @Override
    public int hashCode() {
        return Objects.hash(path, scopeId);
    }

    @Override
    public String toString() {
        return Strings.apply("%s (%s): %s", path, scopeId, url);
    }
}
