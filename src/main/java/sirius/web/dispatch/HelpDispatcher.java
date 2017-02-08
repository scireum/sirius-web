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
import sirius.kernel.di.std.ConfigValue;
import sirius.kernel.di.std.Register;
import sirius.web.http.WebContext;
import sirius.web.http.WebDispatcher;

import java.io.File;
import java.net.URL;
import java.util.List;

/**
 * Takes care of all /help URIs and sub-uris.
 */
@Register
public class HelpDispatcher implements WebDispatcher {

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
        String helpSystemLanguageDirectory = getHelpSystemLanguageDirectory(uri);
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
            ctx.respondWith().cached().nlsTemplate(uri, helpSystemLanguageDirectory);
        }
        ctx.enableTiming("/help/");
        return true;
    }

    public String getHelpSystemLanguageDirectory(String uri) {
        String helpSystemLanguageDirectory = "";
        for (String language : helpSystemLanguageDirectories) {
            String subUri = uri.substring("/help/".length());
            if ((language).equals(subUri) || (language + "/").equals(subUri) || subUri.startsWith(language + "/")) {
                helpSystemLanguageDirectory = language;
            }
        }
        return helpSystemLanguageDirectory;
    }

    public String getRequestedURI(WebContext ctx) {
        String uri = ctx.getRequestedURI();
        if ("/help".equals(uri) || "/help/".equals(uri)) {
            uri = "/help/" + indexTemplate;
        }
        String subUri = uri.substring("/help/".length());
        for (String language : helpSystemLanguageDirectories) {
            if ((language).equals(subUri) || (language + "/").equals(subUri)) {
                uri = "/help/" + language + "/" + indexTemplate;
            }
        }
        return uri;
    }
}
