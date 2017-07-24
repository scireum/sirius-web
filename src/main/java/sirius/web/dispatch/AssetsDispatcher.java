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
import sirius.tagliatelle.Tagliatelle;
import sirius.tagliatelle.Template;
import sirius.tagliatelle.compiler.CompileException;
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
    public boolean preDispatch(WebContext ctx) throws Exception {
        return false;
    }

    @Override
    public boolean dispatch(WebContext ctx) throws Exception {
        if (!ctx.getRequest().uri().startsWith("/assets") || !HttpMethod.GET.equals(ctx.getRequest().method())) {
            return false;
        }

        String uri = getEffectiveURI(ctx);

        if (tryStaticResource(ctx, uri)) {
            return true;
        }

        if (trySASS(ctx, uri)) {
            return true;
        }

        return tryTagliatelle(ctx, uri);
    }

    private boolean tryStaticResource(WebContext ctx, String uri) throws URISyntaxException, IOException {
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
        return false;
    }

    private String getEffectiveURI(WebContext ctx) {
        String uri = ctx.getRequestedURI();
        if (uri.startsWith("/assets/dynamic")) {
            uri = uri.substring(16);
            Tuple<String, String> pair = Strings.split(uri, "/");
            uri = "/assets/" + pair.getSecond();
        }

        return uri;
    }

    private boolean tryTagliatelle(WebContext ctx, String uri) {
        try {
            Optional<Template> template = tagliatelle.resolve(uri + ".pasta");
            if (template.isPresent()) {
                Response response = ctx.respondWith().cached();
                if (template.get().isConstant() && response.handleIfModifiedSince(template.get()
                                                                                          .getCompilationTimestamp())) {
                } else {
                    response.template(HttpResponseStatus.OK, template.get());
                }
                return true;
            }
        } catch (CompileException e) {
            ctx.respondWith().error(HttpResponseStatus.INTERNAL_SERVER_ERROR, Exceptions.handle(Templates.LOG, e));
            return true;
        }

        return false;
    }

    private boolean trySASS(WebContext ctx, String uri) {
        if (!uri.endsWith(".css")) {
            return false;
        }

        String scopeId = UserContext.getCurrentScope().getScopeId();

        String scssUri = uri.substring(0, uri.length() - 4) + ".scss";
        if (resources.resolve(scssUri).isPresent()) {
            ctx.enableTiming("/assets/*.css");
            handleSASS(ctx, uri, scssUri, scopeId);
            return true;
        }

        return false;
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

    /*
     * Uses server-sass to compile a SASS file (.scss) into a .css file
     */
    private void handleSASS(WebContext ctx, String uri, String scssUri, String scopeId) {
        String cacheKey = scopeId + "-" + Files.toSaneFileName(uri.substring(1)).orElse("");
        File file = new File(getCacheDirFile(), cacheKey);

        Optional<Resource> resource = resources.resolve(scssUri);
        if (!resource.isPresent()) {
            return;
        }

        Resource resolved = resource.get();
        if (Sirius.isStartedAsTest() || !file.exists() || file.lastModified() < resolved.getLastModified()) {
            try {
                Resources.LOG.FINE("Compiling: " + scssUri);
                SIRIUSGenerator gen = new SIRIUSGenerator();
                gen.importStylesheet(scssUri);
                gen.compile();
                try (FileWriter writer = new FileWriter(file, false)) {
                    // Let the content compressor take care of minifying the CSS
                    Output out = new Output(writer, false);
                    gen.generate(out);
                }
            } catch (Exception t) {
                if (!file.delete()) {
                    Templates.LOG.WARN("Cannot delete temporary file: %s", file.getAbsolutePath());
                }
                ctx.respondWith().error(HttpResponseStatus.INTERNAL_SERVER_ERROR, Exceptions.handle(Templates.LOG, t));
                return;
            }
        }

        ctx.respondWith().
                named(uri.substring(uri.lastIndexOf("/") + 1)).file(file);
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
