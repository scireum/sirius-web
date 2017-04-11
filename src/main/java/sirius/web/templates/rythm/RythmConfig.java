/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates.rythm;

import com.google.common.collect.Maps;
import org.rythmengine.Rythm;
import org.rythmengine.RythmEngine;
import org.rythmengine.conf.RythmConfigurationKey;
import sirius.kernel.Lifecycle;
import sirius.kernel.async.CallContext;
import sirius.kernel.commons.Files;
import sirius.kernel.di.std.Register;
import sirius.kernel.health.Log;
import sirius.kernel.info.Product;
import sirius.kernel.nls.NLS;
import sirius.web.http.WebContext;
import sirius.web.http.WebServer;
import sirius.web.security.UserContext;

import java.io.File;
import java.lang.reflect.Field;
import java.util.Map;
import java.util.concurrent.ThreadPoolExecutor;

/**
 * Initializes and configures Rythm (http://www.rythmengine.org).
 * <p>
 * Configures Rythm, so that {@literal @}i18n uses {@link NLS#get(String)}. Also the template lookup is changed
 * to scan resources/view/... or resources/help/...
 * <p>
 * Each template will have two auto-import: {@link NLS} and {@link sirius.kernel.commons.Strings}. Additionally,
 * the following variables are declared:
 * <ul>
 * <li><b>ctx</b>: the current {@link CallContext}</li>
 * <li><b>user</b>: the current {@link UserContext}</li>
 * <li><b>prefix</b>: the http url prefix</li>
 * <li><b>product</b>: the name of the product</li>
 * <li><b>version</b>: the version of the product</li>
 * <li><b>detailedVersion</b>: the detailed version of the product</li>
 * <li><b>isDev</b>: <tt>true</tt> if the system is started in development mode, <tt>false</tt> otherwise</li>
 * <li><b>call</b>: the current {@link WebContext}</li>
 * <li><b>template</b>: the name of the template currently being rendered</li>
 * <li><b>lang</b>: the current language</li>
 * <li><b>dateFormat</b>: the date format for the current language</li>
 * <li><b>timeFormat</b>: the time format for the current language</li>
 * </ul>
 */
@Register
public class RythmConfig implements Lifecycle {

    /**
     * Contains the logger for all rythm related output
     */
    public static final Log LOG = Log.get("rythm");

    @Override
    public int getPriority() {
        return WebServer.LIFECYCLE_PRIORITY - 100;
    }

    @Override
    public void started() {
        Map<String, Object> config = Maps.newTreeMap();
        // We always put Rythm in dev mode to support dynamic reloading of templates...
        config.put("rythm.engine.mode", "dev");
        File tmpDir = new File(System.getProperty("java.io.tmpdir"),
                               Files.toSaneFileName(Product.getProduct().getName()).orElse("sirius")
                               + "_"
                               + Files.toSaneFileName(CallContext.getNodeName()).orElse("node")
                               + "_rythm");
        tmpDir.mkdirs();
        deleteTempFiles(tmpDir);
        config.put("rythm.home.tmp.dir", tmpDir.getAbsolutePath());
        config.put("rythm.i18n.message.resolver.impl", I18nResourceResolver.class.getName());
        config.put(RythmConfigurationKey.RESOURCE_LOADER_IMPLS.getKey(), new SiriusResourceLoader());
        config.put(RythmConfigurationKey.CODEGEN_SOURCE_CODE_ENHANCER.getKey(), new SiriusSourceCodeEnhancer());
        Rythm.init(config);
        Rythm.engine().registerFastTag(new IncludeExtensions());
        Rythm.engine().registerTransformer(EscapeStringTransformer.class);
    }

    private void deleteTempFiles(File tmpDir) {
        // Delete all templates on startup to force a clean recompile - otherwise *SOMETIMES* old
        // templates might get used :-/
        if (tmpDir.listFiles() == null) {
            return;
        }
        for (File file : tmpDir.listFiles()) {
            if (file.getName().endsWith(".java") || file.getName().endsWith(".rythm")) {
                if (!file.delete()) {
                    LOG.WARN("Cannot delete: %s", file.getAbsolutePath());
                }
            }
        }
    }

    @Override
    public void stopped() {
        // This is a dirrty hack as the Rythm Engine starts a thread pool which is not shutdown on engine
        // shutdown. As long as this bug is not fixed in rythm, we need to rely on this hack :-(
        // Date: 18.09.14 / Rythm 1.0
        try {
            Field checkerField = RythmEngine.class.getDeclaredField("nonExistsTemplatesChecker");
            checkerField.setAccessible(true);
            Object checker = checkerField.get(Rythm.engine());
            if (checker != null) {
                Field schedulerField = checker.getClass().getDeclaredField("scheduler");
                schedulerField.setAccessible(true);
                ((ThreadPoolExecutor) schedulerField.get(checker)).shutdown();
            }
        } catch (Exception e) {
            LOG.WARN("Cannot halt ThreadPoolExecutor of NonExistsTemplatesChecker (Rythm): " + e.getMessage() + " (" + e
                    .getClass()
                    .getSimpleName() + ")");
        }

        // Shut down rest of rythm...
        Rythm.shutdown();
    }

    @Override
    public void awaitTermination() {
        // Not supported by rythm...
    }

    @Override
    public String getName() {
        return "Rythm-Engine";
    }
}
