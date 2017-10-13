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
import sirius.kernel.di.std.ConfigValue;
import sirius.kernel.di.std.Part;
import sirius.kernel.di.std.Register;
import sirius.tagliatelle.Tagliatelle;
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

    private static final String HELP_PREFIX = "/help";
    private static Pattern startPagePattern = Pattern.compile("^/help/(..)/?$");

    @ConfigValue("help.indexTemplate")
    private String indexTemplate;

    @ConfigValue("help.languages")
    private List<String> helpSystemLanguageDirectories;

    @Part
    private Tagliatelle tagliatelle;

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
        String uri = getRequestedURI(ctx);
        String lang = getHelpSystemLanguageDirectory(uri);
        if (Strings.isFilled(lang)) {
            CallContext.getCurrent().setLang(lang);
        }
        String helpSystemHomeURI = HELP_PREFIX + "/" + lang;
        ctx.setAttribute("helpSystemHomeURI", helpSystemHomeURI);
        if (uri.contains(".")) {
            URL url = getClass().getResource(uri);
            if (url == null) {
                return false;
            } else if ("file".equals(url.getProtocol())) {
                ctx.respondWith().file(new File(url.toURI()));
            } else if (uri.endsWith("html") || uri.endsWith("pasta")) {
                ctx.respondWith().cached().template(uri);
            } else {
                ctx.respondWith().resource(url.openConnection());
            }
        } else {
            //search for the matching template
            StringBuilder sb = new StringBuilder(uri);
            sb.append(".html");
            if (tagliatelle.resolve(uri + ".html.pasta").isPresent()) {
                sb.append(".pasta");
            }
            ctx.respondWith().cached().template(sb.toString());
        }
        ctx.enableTiming(helpSystemHomeURI);
        return true;
    }

    private String getHelpSystemLanguageDirectory(String uri) {
        String subUri = uri.substring((HELP_PREFIX + "/").length()).split("/")[0];
        return helpSystemLanguageDirectories.contains(subUri) ? subUri : "";
    }

    private String getRequestedURI(WebContext ctx) {
        String uri = ctx.getRequestedURI();
        if (HELP_PREFIX.equals(uri) || (HELP_PREFIX + "/").equals(uri)) {
            return buildHomeURI(null);
        }
        Matcher matcher = startPagePattern.matcher(uri);
        if (matcher.matches()) {
            String lang = matcher.group(1);
            if (helpSystemLanguageDirectories.contains(lang)) {
                return buildHomeURI(lang);
            }
        }
        return uri;
    }

    private String buildHomeURI(String lang) {
        String uri = HELP_PREFIX;
        if (Strings.isFilled(lang)) {
            uri = uri + "/" + lang;
        }
        return uri + "/" + indexTemplate;
    }
}
