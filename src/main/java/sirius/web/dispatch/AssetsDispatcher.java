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
import sirius.kernel.Sirius;
import sirius.kernel.async.CallContext;
import sirius.kernel.commons.Files;
import sirius.kernel.commons.PriorityCollector;
import sirius.kernel.commons.Strings;
import sirius.kernel.commons.Tuple;
import sirius.kernel.di.std.ConfigValue;
import sirius.kernel.di.std.Part;
import sirius.kernel.di.std.Register;
import sirius.kernel.health.Exceptions;
import sirius.kernel.health.Log;
import sirius.kernel.info.Product;
import sirius.kernel.settings.Extension;
import sirius.pasta.noodle.compiler.CompileException;
import sirius.pasta.tagliatelle.Tagliatelle;
import sirius.pasta.tagliatelle.Template;
import sirius.web.http.Response;
import sirius.web.http.WebContext;
import sirius.web.http.WebDispatcher;
import sirius.web.resources.Resource;
import sirius.web.resources.Resources;
import sirius.web.security.UserContext;
import sirius.web.templates.Templates;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.Arrays;
import java.util.Optional;

/**
 * Dispatches all URLs below {@code /assets}.
 * <p>
 * All assets are fetched from the classpath and should be located in the <tt>resources</tt> source root (below the
 * <tt>assets</tt> directory).
 * <p>
 * This dispatcher tries to support caching as well as zero-copy delivery of static files if possible.
 */
@Register(classes = {AssetsDispatcher.class, WebDispatcher.class})
public class AssetsDispatcher implements WebDispatcher {

    private static final String ASSETS_PREFIX = "/assets/";

    @ConfigValue("http.generated-directory")
    private String cacheDir;
    private File cacheDirFile;

    @Part
    private Resources resources;

    @Part
    private Tagliatelle tagliatelle;

    private static final Log SASS_LOG = Log.get("sass");

    @Override
    public int getPriority() {
        return PriorityCollector.DEFAULT_PRIORITY - 10;
    }

    @Override
    public DispatchDecision dispatch(WebContext ctx) throws Exception {
        if (!ctx.getRequestedURI().startsWith("/assets") || !HttpMethod.GET.equals(ctx.getRequest().method())) {
            return DispatchDecision.CONTINUE;
        }

        Tuple<String, Integer> uriAndCacheFlag = getEffectiveURI(ctx);

        Response response = ctx.respondWith().cachedForSeconds(uriAndCacheFlag.getSecond());
        DispatchDecision staticResourceDecision = tryStaticResource(ctx, uriAndCacheFlag.getFirst(), response);
        if (staticResourceDecision == DispatchDecision.DONE) {
            return DispatchDecision.DONE;
        }

        DispatchDecision sassDecision = trySASS(ctx, uriAndCacheFlag.getFirst(), response);
        if (sassDecision == DispatchDecision.DONE) {
            return DispatchDecision.DONE;
        }

        return tryTagliatelle(ctx, uriAndCacheFlag.getFirst(), response);
    }

    private DispatchDecision tryStaticResource(WebContext ctx, String uri, Response response)
            throws URISyntaxException, IOException {
        Optional<Resource> res = resources.resolve(uri);
        if (res.isPresent()) {
            ctx.enableTiming(ASSETS_PREFIX);
            URL url = res.get().getUrl();
            if ("file".equals(url.getProtocol())) {
                response.file(new File(url.toURI()));
            } else {
                response.resource(url.openConnection());
            }
            return DispatchDecision.DONE;
        }

        return DispatchDecision.CONTINUE;
    }

    private Tuple<String, Integer> getEffectiveURI(WebContext ctx) {
        String uri = ctx.getRequestedURI();
        if (uri.startsWith("/assets/dynamic")) {
            uri = uri.substring(16);
            Tuple<String, String> pair = Strings.split(uri, "/");
            return Tuple.create(ASSETS_PREFIX + pair.getSecond(), Response.HTTP_CACHE_INFINITE);
        }

        if (uri.startsWith("/assets/no-cache")) {
            return Tuple.create(ASSETS_PREFIX + uri.substring(17), 0);
        }

        return Tuple.create(uri, Response.HTTP_CACHE);
    }

    private DispatchDecision tryTagliatelle(WebContext ctx, String uri, Response response) {
        try {
            Optional<Template> template = tagliatelle.resolve(uri + ".pasta");
            if (template.isPresent()) {
                if (!handleUnmodified(template.get(), response)) {
                    response.template(HttpResponseStatus.OK, template.get());
                }

                return DispatchDecision.DONE;
            }
        } catch (CompileException e) {
            ctx.respondWith().error(HttpResponseStatus.INTERNAL_SERVER_ERROR, Exceptions.handle(Templates.LOG, e));
            return DispatchDecision.DONE;
        }

        return DispatchDecision.CONTINUE;
    }

    private boolean handleUnmodified(Template template, Response response) {
        if (!template.isConstant()) {
            return false;
        }

        return response.handleIfModifiedSince(template.getCompilationTimestamp());
    }

    private DispatchDecision trySASS(WebContext ctx, String uri, Response response) {
        if (!uri.endsWith(".css")) {
            return DispatchDecision.CONTINUE;
        }

        String scopeId = UserContext.getCurrentScope().getScopeId();

        String scssUri = uri.substring(0, uri.length() - 4) + ".scss";
        Optional<Resource> resource = resources.resolve(scssUri);
        if (resource.isEmpty()) {
            return DispatchDecision.CONTINUE;
        }

        ctx.enableTiming("/assets/*.css");
        String cacheKey = scopeId + "-" + Files.toSaneFileName(uri.substring(1)).orElse("");
        File file = new File(getCacheDirFile(), cacheKey);

        if (Sirius.isStartedAsTest() || Sirius.isDev() || !file.exists() || file.lastModified() < resource.get()
                                                                                                          .getLastModified()) {
            try {
                compileSASS(scssUri, file);
            } catch (Exception t) {
                Files.delete(file);
                ctx.respondWith().error(HttpResponseStatus.INTERNAL_SERVER_ERROR, Exceptions.handle(Templates.LOG, t));
                return DispatchDecision.DONE;
            }
        }

        response.named(uri.substring(uri.lastIndexOf('/') + 1)).file(file);
        return DispatchDecision.DONE;
    }

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
    }

    private void compileSASS(String scssUri, File file) throws IOException {
        Resources.LOG.FINE("Compiling: " + scssUri);
        SIRIUSGenerator gen = new SIRIUSGenerator();
        gen.importStylesheet(scssUri);

        String configPath = "assets.scss." + Files.getFilenameWithoutExtension(scssUri);
        if (Sirius.getSettings().has(configPath)) {
            for (Extension extension : Sirius.getSettings().getExtensions(configPath)) {
                gen.importStylesheet(extension.get("path").asString());
            }
        }

        gen.compile();
        try (FileWriter writer = new FileWriter(file, false)) {
            // Let the content compressor take care of minifying the CSS
            Output out = new Output(writer, false);
            gen.generate(out);
        }
    }

    /**
     * Flushes all cached and pre-computed scss / css files.
     * <p>
     * This can be used in environments where the scss files change due to included files. This isn't detected by the
     * framework, as we only check the main file.
     */
    public void flushCompiledSCSS() {
        try {
            Arrays.stream(getCacheDirFile().listFiles())
                  .filter(f -> f.getName().endsWith(".css"))
                  .forEach(File::delete);
        } catch (NullPointerException e) {
            // Happens if the directy does not exist....
            Exceptions.ignore(e);
        } catch (Exception e) {
            Exceptions.handle(e);
        }
    }

    /*
     * Resolves the directory used to cache the generated files
     */
    private File getCacheDirFile() {
        if (cacheDirFile == null) {
            File tmpDir = new File(System.getProperty("java.io.tmpdir"),
                                   Files.toSaneFileName(Product.getProduct().getName()).orElse("sirius")
                                   + "_"
                                   + Files.toSaneFileName(CallContext.getNodeName()).orElse("node")
                                   + "_"
                                   + cacheDir);
            tmpDir.mkdirs();
            cacheDirFile = tmpDir;
        }
        return cacheDirFile;
    }
}
