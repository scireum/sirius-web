/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates.rythm;

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.typesafe.config.Config;
import org.rythmengine.extension.ISourceCodeEnhancer;
import org.rythmengine.template.ITemplate;
import sirius.kernel.Sirius;
import sirius.kernel.async.CallContext;
import sirius.kernel.di.std.Parts;
import sirius.kernel.info.Product;
import sirius.kernel.nls.NLS;
import sirius.web.http.WebContext;
import sirius.web.security.UserContext;

import java.time.LocalDate;
import java.util.Collection;
import java.util.List;
import java.util.Map;

/**
 * Applies {@link RythmExtension} instances found as <tt>parts</tt> to the rythm engine.
 */
class SiriusSourceCodeEnhancer implements ISourceCodeEnhancer {

    @Parts(RythmExtension.class)
    private static Collection<RythmExtension> extensions;

    @Override
    public List<String> imports() {
        List<String> result = Lists.newArrayList();
        result.add("sirius.kernel.commons.Strings");
        result.add("sirius.kernel.nls.NLS");
        result.add("sirius.web.controller.Page");
        result.add("java.util.*");

        return result;
    }

    @Override
    public String sourceCode() {
        return "";
    }

    @Override
    public Map<String, ?> getRenderArgDescriptions() {
        final Map<String, Object> map = Maps.newTreeMap();
        map.put("ctx", CallContext.class);
        map.put("user", UserContext.class);
        map.put("prefix", String.class);
        map.put("product", String.class);
        map.put("year", int.class);
        map.put("detailedVersion", String.class);
        map.put("isDev", Boolean.class);
        map.put("call", WebContext.class);
        map.put("template", String.class);
        map.put("lang", String.class);
        map.put("dateFormat", String.class);
        map.put("timeFormat", String.class);
        map.put("config", Config.class);

        for (RythmExtension ext : extensions) {
            ext.collectExtensionNames(entity -> map.put(entity.getFirst(), entity.getSecond()));
        }
        return map;
    }

    @Override
    public void setRenderArgs(final ITemplate template) {
        CallContext ctx = CallContext.getCurrent();
        String url = template.__getName();
        if (template instanceof URLTemplateResource) {
            url = ((URLTemplateResource) template.__getTemplateClass(true).templateResource).getUrl();
        }
        ctx.addToMDC("template", url);
        WebContext wc = ctx.get(WebContext.class);

        template.__setRenderArg("ctx", ctx);
        template.__setRenderArg("user", ctx.get(UserContext.class));
        template.__setRenderArg("prefix", WebContext.getContextPrefix());
        template.__setRenderArg("product", Product.getProduct().getName());
        template.__setRenderArg("year", LocalDate.now().getYear());
        template.__setRenderArg("detailedVersion", Product.getProduct().getDetails());
        template.__setRenderArg("isDev", Sirius.isDev());
        template.__setRenderArg("call", wc);
        template.__setRenderArg("template", url);
        template.__setRenderArg("lang", NLS.getCurrentLang());
        template.__setRenderArg("dateFormat", NLS.get("RythmConfig.jsDateFormat"));
        template.__setRenderArg("timeFormat", NLS.get("RythmConfig.jsTimeFormat"));
        template.__setRenderArg("config", Sirius.getConfig());

        for (RythmExtension ext : extensions) {
            ext.collectExtensionValues(entity -> template.__setRenderArg(entity.getFirst(), entity.getSecond()));
        }
    }
}
