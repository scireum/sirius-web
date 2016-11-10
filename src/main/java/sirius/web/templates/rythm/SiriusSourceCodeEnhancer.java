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
import org.rythmengine.extension.ISourceCodeEnhancer;
import org.rythmengine.template.ITemplate;
import sirius.kernel.Sirius;
import sirius.kernel.async.CallContext;
import sirius.kernel.di.std.Parts;

import java.util.Collection;
import java.util.Collections;
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
        map.put("template", String.class);
        map.put("renderArgs", Map.class);

        extensions.forEach(ext -> ext.collectExtensionNames(map::put));

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
        template.__setRenderArg("template", url);

        // In developement mode, we provide a map of all implicitely defined parameters
        // to support developers.
        if (Sirius.isDev()) {
            final Map<String, Object> map = Maps.newTreeMap();
            extensions.forEach(ext -> ext.collectExtensionValues(map::put));
            map.entrySet().forEach(entry -> template.__setRenderArg(entry.getKey(), entry.getValue()));
            template.__setRenderArg("renderArgs", map);
        } else {
            extensions.forEach(ext -> ext.collectExtensionValues(template::__setRenderArg));
            template.__setRenderArg("renderArgs", Collections.emptyMap());
        }
    }
}
