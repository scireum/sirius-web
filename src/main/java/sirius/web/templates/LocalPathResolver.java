/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates;

import sirius.kernel.commons.RateLimit;
import sirius.kernel.commons.Strings;
import sirius.kernel.di.std.ConfigValue;
import sirius.kernel.di.std.Register;
import sirius.kernel.health.Exceptions;
import sirius.web.security.UserContext;

import java.io.File;
import java.net.MalformedURLException;
import java.util.concurrent.TimeUnit;

/**
 * Tries to resolve a given path into a file locally stored on disk.
 * <p>
 * This can be used to store and apply customer specific changes. The base directory is determined via the
 * system configuration (<tt>content.localResourcePath</tt>). If this value is an empty string, the resolver
 * will be disabled.
 * </p>
 *
 * @author Andreas Haufler (aha@scireum.de)
 * @since 2014/07
 */
@Register
public class LocalPathResolver implements Resolver {

    @ConfigValue("content.localResourcePath")
    private String localResourcePath;
    private File baseDir;
    private Boolean baseDirFound;
    private RateLimit checkLimit = RateLimit.timeInterval(1, TimeUnit.MINUTES);

    private File getBaseDir() {
        if (baseDir == null) {
            baseDir = new File(localResourcePath);
        }

        return baseDir;
    }

    @Override
    public Resource resolve(String scopeId, String resource) {
        if (!isReady()) {
            return null;
        }
        String scope = UserContext.getCurrentScope().getScopeId();
        File file = new File(baseDir,
                             (scope + "/" + (resource.startsWith("/") ? resource : "/" + resource)).replace("/",
                                                                                                            File.separator)
        );
        if (file.exists()) {
            try {
                return Resource.dynamicResource(scopeId, resource, file.toURI().toURL());
            } catch (MalformedURLException e) {
                Exceptions.handle(e);
                return null;
            }
        }
        return null;
    }

    @Override
    public int getPriority() {
        return 50;
    }

    /*
     * Determines if the system is ready - this requires the base dir to exist.
     */
    private boolean isReady() {
        if (baseDirFound == null || (!baseDirFound && checkLimit.check())) {
            if (Strings.isEmpty(localResourcePath)) {
                baseDirFound = false;
            } else {
                getBaseDir();
                baseDirFound = baseDir.exists();
                if (!baseDirFound) {
                    Content.LOG.WARN(
                            "Base directory: '%s' (%s) for local templates does not exist! Will check every minute...",
                            localResourcePath,
                            baseDir.getAbsolutePath());
                }
            }
        }
        return baseDirFound;
    }
}
