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
import sirius.kernel.async.CallContext;
import sirius.kernel.commons.PriorityCollector;
import sirius.kernel.commons.Strings;
import sirius.kernel.commons.Tuple;
import sirius.kernel.di.std.ConfigValue;
import sirius.kernel.di.std.Part;
import sirius.kernel.di.std.Register;
import sirius.kernel.health.Exceptions;
import sirius.kernel.nls.NLS;
import sirius.tagliatelle.Tagliatelle;
import sirius.tagliatelle.Template;
import sirius.tagliatelle.compiler.CompileException;
import sirius.web.controller.Message;
import sirius.web.http.WebContext;
import sirius.web.http.WebDispatcher;
import sirius.web.resources.Resource;
import sirius.web.resources.Resources;
import sirius.web.security.UserContext;

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

    @Part
    private Tagliatelle tagliatelle;

    @Override
    public int getPriority() {
        return PriorityCollector.DEFAULT_PRIORITY + 100;
    }

    @Override
    public DispatchDecision dispatch(WebContext ctx) throws Exception {
        if (!ctx.getRequestedURI().startsWith(HELP_PREFIX) || !HttpMethod.GET.equals(ctx.getRequest().method())) {
            return DispatchDecision.CONTINUE;
        }

        ctx.enableTiming(HELP_PREFIX);

        String uri = ctx.getRequestedURI();
        if (uri.endsWith("/")) {
            uri = uri.substring(0, uri.length() - 1);
        }

        if (HELP_PREFIX.equals(uri)) {
            return serveTopic(ctx, HELP_PREFIX + "/" + indexTemplate);
        }

        if (uri.contains(".") && !uri.endsWith(PASTA_SUFFIX)) {
            return serveAsset(ctx, uri);
        }

        // cut /help/ and try to extract the locale
        Tuple<String, String> langAndTopic = Strings.split(uri.substring(HELP_PREFIX_LENGTH), "/");
        boolean languageFound = setupLang(langAndTopic.getFirst());

        if (!languageFound || Strings.isFilled(langAndTopic.getSecond())) {
            DispatchDecision topicDecision = serveTopic(ctx, uri);
            if (topicDecision == DispatchDecision.DONE) {
                return DispatchDecision.DONE;
            }
        }

        if (!languageFound || Strings.areEqual(langAndTopic.getFirst(), NLS.getDefaultLanguage())) {
            return serveTopic(ctx, HELP_PREFIX + "/" + indexTemplate);
        } else {
            return serveTopic(ctx, HELP_PREFIX + "/" + langAndTopic.getFirst() + "/" + indexTemplate);
        }
    }

    private DispatchDecision serveAsset(WebContext ctx, String uri) throws IOException {
        Resource asset = resources.resolve(uri).orElse(null);
        if (asset == null) {
            return DispatchDecision.CONTINUE;
        }

        ctx.respondWith().resource(asset.getUrl().openConnection());
        return DispatchDecision.DONE;
    }

    private DispatchDecision serveTopic(WebContext ctx, String uri) {
        Template template = resolveTemplate(uri);
        if (template != null) {
            ctx.respondWith().cached().template(HttpResponseStatus.OK, template);
            return DispatchDecision.DONE;
        }

        UserContext.get().addMessage(Message.error(NLS.get("HelpDispatcher.unknownTopic")));
        return DispatchDecision.CONTINUE;
    }

    private Template resolveTemplate(String uri) {
        try {
            return tagliatelle.resolve(uri.endsWith(PASTA_SUFFIX) ? uri : uri + PASTA_SUFFIX).orElse(null);
        } catch (CompileException e) {
            Exceptions.handle()
                      .to(Tagliatelle.LOG)
                      .error(e)
                      .withSystemErrorMessage("Failed to render the template '%s': %s (%s)", uri)
                      .handle();
            return null;
        }
    }

    private boolean setupLang(String lang) {
        if (LANG_PATTERN.matcher(lang).matches() && helpSystemLanguageDirectories.contains(lang)) {
            CallContext.getCurrent().setLang(lang);
            return true;
        }

        return false;
    }
}
