/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.dispatch;

import io.netty.handler.codec.http.HttpMethod;
import io.netty.handler.codec.http.HttpResponseStatus;
import org.serversass.Generator;
import org.serversass.Output;
import org.serversass.ast.Value;
import sirius.kernel.Sirius;
import sirius.kernel.async.CallContext;
import sirius.kernel.commons.Files;
import sirius.kernel.commons.PriorityCollector;
import sirius.kernel.commons.Strings;
import sirius.kernel.commons.Tuple;
import sirius.kernel.di.std.ConfigValue;
import sirius.kernel.di.std.Part;
import sirius.kernel.di.std.PriorityParts;
import sirius.kernel.di.std.Register;
import sirius.kernel.health.Exceptions;
import sirius.kernel.health.Log;
import sirius.kernel.info.Product;
import sirius.web.http.WebContext;
import sirius.web.http.WebDispatcher;
import sirius.web.security.UserContext;
import sirius.web.templates.Resolver;
import sirius.web.templates.Resource;
import sirius.web.templates.Resources;
import sirius.web.templates.Templates;

import java.io.File;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.List;
import java.util.Optional;

/**
 * Dispatches all URLs below {@code /assets}.
 * <p>
 * All assets are fetched from the classpath and should be located in the <tt>resources</tt> source root (below the
 * <tt>assets</tt> directory).
 * <p>
 * This dispatcher tries to support caching as well as zero-copy delivery of static files if possible.
 */
@Register
public class AssetsDispatcher implements WebDispatcher {

    @Override
    public int getPriority() {
        return PriorityCollector.DEFAULT_PRIORITY - 10;
    }

    @Override
    public boolean preDispatch(WebContext ctx) throws Exception {
        return false;
    }

    @ConfigValue("http.generated-directory")
    private String cacheDir;
    private File cacheDirFile;

    @PriorityParts(Resolver.class)
    private List<Resolver> resolvers;

    @Override
    public boolean dispatch(WebContext ctx) throws Exception {
        if (!ctx.getRequest().uri().startsWith("/assets") || HttpMethod.GET != ctx.getRequest().method()) {
            return false;
        }
        // The real dispatching is put into its own method to support inlining of this check by the JIT
        return doDispatch(ctx);
    }

    /*
     * Actually tries to dispatch the incoming request which starts with /assets....
     */
    private boolean doDispatch(WebContext ctx) throws URISyntaxException, IOException {
        String uri = ctx.getRequestedURI();
        if (uri.startsWith("/assets/dynamic")) {
            uri = uri.substring(16);
            Tuple<String, String> pair = Strings.split(uri, "/");
            uri = "/assets/" + pair.getSecond();
        }
        Optional<Resource> res = resources.resolve(uri);
        if (res.isPresent()) {
            ctx.enableTiming("/assets/");
            URL url = res.get().getUrl();
            if ("file".equals(url.getProtocol())) {
                ctx.respondWith().file(new File(url.toURI()));
            } else {
                ctx.respondWith().resource(url.openConnection());
            }
            return true;
        }

        String scopeId = UserContext.getCurrentScope().getScopeId();

        // If the file is not found not is a .css file, check if we need to generate it via a .scss file
        if (uri.endsWith(".css")) {
            ctx.enableTiming("/assets/*.css");
            String scssUri = uri.substring(0, uri.length() - 4) + ".scss";
            if (resources.resolve(scssUri).isPresent()) {
                handleSASS(ctx, uri, scssUri, scopeId);
                return true;
            }
        }
        // If the file is non existent, check if we can generate it by using a velocity template
        if (resources.resolve(uri + ".vm").isPresent()) {
            ctx.enableTiming("/assets/*.vm");
            handleVM(ctx, uri, scopeId);
            return true;
        }
        return false;
    }

    private static final Log SASS_LOG = Log.get("sass");

    /*
     * Subclass of generator which takes care of proper logging
     */
    private class SIRIUSGenerator extends Generator {
        @Override
        public void debug(String message) {
            SASS_LOG.FINE(message);
        }

        @Override
        public void warn(String message) {
            SASS_LOG.WARN(message);
        }

        @Override
        protected InputStream resolveIntoStream(String sheet) throws IOException {
            Optional<Resource> res = resources.resolve(sheet);
            if (res.isPresent()) {
                return res.get().getUrl().openStream();
            }
            return null;
        }

        SIRIUSGenerator() {
            scope.set("prefix", new Value(WebContext.getContextPrefix()));
        }
    }

    @Part
    private Resources resources;
    @Part
    private Templates templates;

    /*
     * Helper interface to factor the content generation by velocity and SASS out of the
     * actual handling of the cache file etc.
     */
    private interface DynamicGenerator {
        /**
         * Creates the desired output into the given cache file
         *
         * @param output the file used to cache the generated contents
         * @throws IOException in case of an IO error
         */
        void generate(File output) throws IOException;
    }

    private void handleByGeneratingFile(WebContext ctx,
                                        String uri,
                                        String resourceName,
                                        String scopeId,
                                        DynamicGenerator generator) {
        String cacheKey = scopeId + "-" + Files.toSaneFileName(uri.substring(1)).orElse("");
        File file = new File(getCacheDirFile(), cacheKey);

        Optional<Resource> resource = resources.resolve(resourceName);
        if (!resource.isPresent()) {
            return;
        }
        Resource resolved = resource.get();
        if (Sirius.isStartedAsTest() || !file.exists() || file.lastModified() < resolved.getLastModified()) {
            try {
                Resources.LOG.FINE("Compiling: " + resourceName);
                generator.generate(file);
            } catch (Throwable t) {
                file.delete();
                ctx.respondWith().error(HttpResponseStatus.INTERNAL_SERVER_ERROR, Exceptions.handle(Templates.LOG, t));
                return;
            }
        }

        ctx.respondWith().
                named(uri.substring(uri.lastIndexOf("/") + 1)).file(file);
    }

    /*
     * Uses Velocity (via the content generator) to generate the desired file
     */
    private void handleVM(WebContext ctx, String uri, String scopeId) {
        handleByGeneratingFile(ctx, uri, uri + ".vm", scopeId, file -> {
            try (FileOutputStream out = new FileOutputStream(file, false)) {
                templates.generator().useTemplate(uri + ".vm").generateTo(out);
            }
        });
    }

    /*
     * Uses server-sass to compile a SASS file (.scss) into a .css file
     */
    private void handleSASS(WebContext ctx, String uri, String scssUri, String scopeId) {
        handleByGeneratingFile(ctx, uri, scssUri, scopeId, file -> {
            SIRIUSGenerator gen = new SIRIUSGenerator();
            gen.importStylesheet(scssUri);
            gen.compile();
            try (FileWriter writer = new FileWriter(file, false)) {
                // Let the content compressor take care of minifying the CSS
                Output out = new Output(writer, false);
                gen.generate(out);
            }
        });
    }

    /*
     * Resolves the directory used to cache the generated files
     */
    private File getCacheDirFile() {
        if (cacheDirFile == null) {
            File tmpDir = new File(System.getProperty("java.io.tmpdir"),
                                   Files.toSaneFileName(Product.getProduct().getName()).orElse("")
                                   + "_"
                                   + Files.toSaneFileName(CallContext.getNodeName())
                                   + "_"
                                   + cacheDir);
            tmpDir.mkdirs();
            cacheDirFile = tmpDir;
        }
        return cacheDirFile;
    }
}
