/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.resources;

import sirius.kernel.Sirius;
import sirius.kernel.commons.RateLimit;
import sirius.kernel.commons.Streams;
import sirius.kernel.commons.Strings;
import sirius.kernel.health.Exceptions;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URISyntaxException;
import java.net.URL;
import java.net.URLConnection;
import java.nio.charset.StandardCharsets;
import java.util.Objects;
import java.util.concurrent.TimeUnit;

/**
 * Wraps an resolved URL as resource.
 * <p>
 * Used as result of a {@link Resolver}. Next to the resolved URL it also stores the original
 * scope and path which was used to resolve the url.
 *
 * @see Resolver
 * @see sirius.web.security.UserContext#getCurrentScope()
 */
public class Resource {
    private static final String PROTOCOL_FILE = "file";
    private final String scopeId;
    private final String path;
    private final URL url;
    private final File file;
    private long lastModified = -1;
    private final boolean consideredConstant;
    private final RateLimit checkInterval = RateLimit.timeInterval(Sirius.isDev() ? 1 : 10, TimeUnit.SECONDS);
    private final long minLastModified;

    private Resource(String scopeId, String path, URL url, boolean constant) {
        this.scopeId = scopeId;
        this.path = path;
        this.url = url;
        this.file = determineFile(url);

        if (file != null && (Sirius.isDev() || Sirius.isStartedAsTest())) {
            ensureCaseMatch(path);
        }

        this.consideredConstant = constant;
        this.minLastModified = System.currentTimeMillis();

        Objects.requireNonNull(scopeId);
        Objects.requireNonNull(path);
        Objects.requireNonNull(url);
    }

    private void ensureCaseMatch(String path) {
        try {
            String absolutePath = file.getAbsolutePath();
            String canonicalPath = file.getCanonicalPath();
            if (!absolutePath.equals(canonicalPath) && absolutePath.equalsIgnoreCase(canonicalPath)) {
                throw new IllegalStateException(Strings.apply(
                        "A resource was found, but only case insensitive: %s vs. %s",
                        path,
                        canonicalPath));
            }
        } catch (IOException e) {
            Exceptions.ignore(e);
        }
    }

    private File determineFile(URL url) {
        if (!PROTOCOL_FILE.equals(url.getProtocol())) {
            return null;
        }
        try {
            File localFile = new File(url.toURI());
            if (localFile.exists()) {
                return localFile;
            } else {
                return null;
            }
        } catch (URISyntaxException e) {
            Exceptions.ignore(e);
            return null;
        }
    }

    /**
     * Creates a new dynamic resource for the given scope, path and resulting url.
     * <p>
     * A dynamic resource might change over time (like a customer specific file on disk). Therefore we
     * frequently check for changes.
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
     *
     * @param scopeId the scope which was used to resolve the resource
     * @param path    the local path pointing to the resource
     * @param url     the url pointing to the actual content
     * @return a new resource based on the given parameters
     */
    public static Resource constantResource(String scopeId, String path, URL url) {
        return new Resource(scopeId, path, url, true);
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
            if (file != null) {
                return new FileInputStream(file);
            }
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
                return Streams.toByteArray(in);
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
        return new String(getContent(), StandardCharsets.UTF_8);
    }

    /**
     * Saves the content of the resource to a file at the given path.
     * <p>
     * This method creates the intermediate parent directories if non existent
     *
     * @param path the path to write the output file
     * @return the output {@link File}
     */
    public File saveContentToFile(String path) {
        File outputFile = new File(path);
        if (!outputFile.getParentFile().exists()) {
            outputFile.getParentFile().mkdirs();
        }
        try (OutputStream outputStream = new FileOutputStream(outputFile); InputStream inputStream = openStream()) {
            Streams.transfer(inputStream, outputStream);
            return outputFile;
        } catch (IOException e) {
            throw Exceptions.handle(e);
        }
    }

    /**
     * Computes the last modified date.
     * <p>
     * This value will be cached if the resource is considered constant. However, in development systems
     * the resource will sill be checked in a regular interval.
     * <p>
     * Also note that the last modified timestamp is
     * at least the timestamp when the resource was resolved. This is required to manage dynamic resources which
     * suddenly are created or vanish.
     *
     * @return the last modified value of the underlying resource.
     */
    public long getLastModified() {
        if (lastModified == -1 || Sirius.isDev() || (!consideredConstant && checkInterval.check())) {
            try {
                if (file != null) {
                    lastModified = file.lastModified();
                } else {
                    determineLastModified();
                }
            } catch (IOException e) {
                Exceptions.handle(e);
                lastModified = System.currentTimeMillis();
            }
        }
        return Math.max(lastModified, minLastModified);
    }

    private void determineLastModified() throws IOException {
        URLConnection c = url.openConnection();
        lastModified = c.getLastModified();
        try {
            // Close the input stream since the stupid implementation of
            // SUN's FileURLConnection always keeps an InputStream open on
            // connect.
            c.getInputStream().close();
        } catch (Exception e) {
            Resources.LOG.WARN(e);
        }
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
