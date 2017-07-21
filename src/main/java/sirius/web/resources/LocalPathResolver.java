/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.resources;

import sirius.kernel.commons.RateLimit;
import sirius.kernel.commons.Strings;
import sirius.kernel.di.std.ConfigValue;
import sirius.kernel.di.std.Register;
import sirius.kernel.health.Exceptions;

import java.io.File;
import java.net.MalformedURLException;
import java.util.concurrent.TimeUnit;

/**
 * Tries to resolve a given path into a file locally stored on disk.
 * <p>
 * This can be used to store and apply customer specific changes. The base directory is determined via the
 * system configuration (<tt>content.localResourcePath</tt>). If this value is an empty string, the resolver
 * will be disabled.
 */
@Register
public class LocalPathResolver implements Resolver {

    private static final String DEFAULT_BASE_DIR = "data/resources";
    private static final String CHECK_MSG =
            "Base directory: '%s' (%s) for local templates does not exist. Will check every minute...";

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
        File file = new File(baseDir, (scopeId + resource).replace("/", File.separator));
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
            synchronized (this) {
                if (Strings.isEmpty(localResourcePath)) {
                    baseDirFound = false;
                } else {
                    getBaseDir();
                    if (!baseDir.exists()) {
                        if (DEFAULT_BASE_DIR.equals(localResourcePath)) {
                            // If we're using the default lookup path, only report once, that it does not exist
                            // as it is not necessary at all.
                            if (baseDirFound == null) {
                                Resources.LOG.INFO(CHECK_MSG, localResourcePath, baseDir.getAbsolutePath());
                            }
                        } else {
                            Resources.LOG.WARN(CHECK_MSG, localResourcePath, baseDir.getAbsolutePath());
                        }
                    }
                    baseDirFound = baseDir.exists();
                }
            }
        }
        return baseDirFound;
    }
}
