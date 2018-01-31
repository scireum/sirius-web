/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.dispatch;

import io.netty.handler.codec.http.HttpMethod;
import sirius.kernel.async.CallContext;
import sirius.kernel.commons.PriorityCollector;
import sirius.kernel.commons.Strings;
import sirius.kernel.commons.Tuple;
import sirius.kernel.di.std.ConfigValue;
import sirius.kernel.di.std.Part;
import sirius.kernel.di.std.Register;
import sirius.kernel.nls.NLS;
import sirius.web.http.WebContext;
import sirius.web.http.WebDispatcher;
import sirius.web.resources.Resource;
import sirius.web.resources.Resources;

import java.io.IOException;
import java.util.List;
import java.util.regex.Pattern;

/**
 * Takes care of all /help URIs and sub-uris.
 */
@Register
public class HelpDispatcher implements WebDispatcher {

    private static final String HELP_PREFIX = "/help";
    private static final int HELP_PREFIX_LENGTH = "/help/".length();
    private static final Pattern LANG_PATTERN = Pattern.compile("[a-z]{2}");
    private static final String PASTA_SUFFIX = ".html.pasta";

    @ConfigValue("help.indexTemplate")
    private String indexTemplate;

    @ConfigValue("help.languages")
    private List<String> helpSystemLanguageDirectories;

    @Part
    private Resources resources;

    @Override
    public boolean preDispatch(WebContext ctx) throws Exception {
        return false;
    }

    @Override
    public int getPriority() {
        return PriorityCollector.DEFAULT_PRIORITY + 100;
    }

    @Override
    public boolean dispatch(WebContext ctx) throws Exception {
        if (!ctx.getRequestedURI().startsWith(HELP_PREFIX) || !HttpMethod.GET.equals(ctx.getRequest().method())) {
            return false;
        }

        ctx.enableTiming(HELP_PREFIX);

        String uri = ctx.getRequestedURI();
        if (uri.endsWith("/")) {
            uri = uri.substring(0, uri.length() - 1);
        }

        if (HELP_PREFIX.equals(uri)) {
            return serveTopic(ctx, HELP_PREFIX + "/" + indexTemplate);
        }

        // cut /help/ and try to extract the locale
        Tuple<String, String> langAndTopic = Strings.split(uri.substring(HELP_PREFIX_LENGTH), "/");
        setupLang(langAndTopic.getFirst());
        if (Strings.isEmpty(langAndTopic.getSecond())) {
            if (Strings.areEqual(langAndTopic.getFirst(), NLS.getDefaultLanguage())) {
                return serveTopic(ctx, HELP_PREFIX + "/" + indexTemplate);
            } else {
                return serveTopic(ctx, HELP_PREFIX + "/" + langAndTopic.getFirst() + "/" + indexTemplate);
            }
        }

        if (uri.contains(".") && !uri.endsWith(PASTA_SUFFIX)) {
            return serveAsset(ctx, uri);
        } else {
            return serveTopic(ctx, uri);
        }
    }

    private boolean serveAsset(WebContext ctx, String uri) throws IOException {
        Resource asset = resources.resolve(uri).orElse(null);
        if (asset == null) {
            return false;
        }

        ctx.respondWith().resource(asset.getUrl().openConnection());
        return true;
    }

    private boolean serveTopic(WebContext ctx, String uri) {
        if (uri.endsWith(PASTA_SUFFIX)) {
            ctx.respondWith().cached().template(uri);
        } else {
            ctx.respondWith().cached().template(uri + PASTA_SUFFIX);
        }
        return true;
    }

    private void setupLang(String lang) {
        if (LANG_PATTERN.matcher(lang).matches() && helpSystemLanguageDirectories.contains(lang)) {
            CallContext.getCurrent().setLang(lang);
        }
    }
}
