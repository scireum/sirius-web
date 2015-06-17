/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates.velocity;

import com.typesafe.config.ConfigValue;
import org.apache.velocity.app.VelocityEngine;
import org.apache.velocity.runtime.RuntimeConstants;
import sirius.kernel.Sirius;
import sirius.kernel.health.Exceptions;
import sirius.web.templates.Templates;

import java.util.Map;

/**
 * Used to start our own Velocity instance so that we don't have to touch the
 * global static one.
 */
public class VelocityHelper {

    private static VelocityEngine engine;

    private VelocityHelper() {
    }

    public static VelocityEngine getEngine() {
        if (engine == null) {
            engine = createVelocityEngine();
        }

        return engine;
    }

    private static VelocityEngine createVelocityEngine() {
        VelocityEngine eng = new VelocityEngine();
        try {
            eng.setProperty("sirius.resource.loader.class", VelocityResourceLoader.class.getName());
            eng.setProperty(RuntimeConstants.RESOURCE_MANAGER_CACHE_CLASS, VelocityResourceCache.class.getName());
            eng.setProperty(RuntimeConstants.RESOURCE_LOADER, "sirius");
            StringBuilder libraryPath = new StringBuilder();
            for (Map.Entry<String, ConfigValue> e : Sirius.getConfig()
                                                          .getConfig("content.velocity-libraries")
                                                          .entrySet()) {
                libraryPath.append(e.getValue().unwrapped());
                libraryPath.append(",");
            }
            eng.setProperty(RuntimeConstants.VM_LIBRARY, libraryPath.toString());
            eng.setProperty(RuntimeConstants.SET_NULL_ALLOWED, Boolean.TRUE);
            eng.setProperty(RuntimeConstants.RUNTIME_LOG_LOGSYSTEM_CLASS,
                            "org.apache.velocity.runtime.log.Log4JLogChute");
            eng.setProperty("runtime.log.logsystem.log4j.logger", "velocity");
            eng.init();

            return eng;
        } catch (Throwable e) {
            throw Exceptions.handle(Templates.LOG, e);
        }
    }
}
