/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.dispatch;

import io.netty.handler.codec.http.HttpMethod;
import sirius.kernel.commons.PriorityCollector;
import sirius.kernel.commons.Strings;
import sirius.kernel.di.std.ConfigValue;
import sirius.kernel.di.std.Register;
import sirius.web.http.WebContext;
import sirius.web.http.WebDispatcher;

import java.io.File;
import java.net.URL;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Takes care of all /help URIs and sub-uris.
 */
@Register
public class HelpDispatcher implements WebDispatcher {

    private static Pattern startPagePattern = Pattern.compile("^/help/(..)/?$");

    @ConfigValue("help.indexTemplate")
    private String indexTemplate;

    @ConfigValue("help.languages")
    private List<String> helpSystemLanguageDirectories;

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
        if (!ctx.getRequest().uri().startsWith("/help") || HttpMethod.GET != ctx.getRequest().method()) {
            return false;
        }
        String uri = getRequestedURI(ctx);
        String helpsystemHomeURI = "/help/" + getHelpSystemLanguageDirectory(uri);
        if (uri.contains(".") && !uri.endsWith("html")) {
            // Dispatch static content...
            URL url = getClass().getResource(uri);
            if (url == null) {
                return false;
            } else if ("file".equals(url.getProtocol())) {
                ctx.respondWith().file(new File(url.toURI()));
            } else {
                ctx.respondWith().resource(url.openConnection());
            }
        } else {
            // Render help template...
            ctx.respondWith().cached().nlsTemplate(uri, helpsystemHomeURI);
        }
        ctx.enableTiming("/help/");
        return true;
    }

    public String getHelpSystemLanguageDirectory(String uri) {
        String subUri = uri.substring("/help/".length()).split("/")[0];
        return helpSystemLanguageDirectories.contains(subUri) ? subUri : "";
    }

    public String getRequestedURI(WebContext ctx) {
        String uri = ctx.getRequestedURI();
        if ("/help".equals(uri) || "/help/".equals(uri)) {
            return buildStartUri(null);
        }
        Matcher matcher = startPagePattern.matcher(uri);
        if (matcher.matches()) {
            String lang = matcher.group(1);
            if (helpSystemLanguageDirectories.contains(lang)) {
                return buildStartUri(lang);
            }
        }
        return uri;
    }

    public String buildStartUri(String lang) {
        String uri = "/help/";
        if (Strings.isFilled(lang)) {
            uri = uri + lang + "/";
        }
        return uri + indexTemplate;
    }
}
