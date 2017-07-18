/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle;

import io.netty.handler.codec.http.HttpResponseStatus;
import sirius.kernel.Sirius;
import sirius.kernel.commons.MultiMap;
import sirius.kernel.commons.Tuple;
import sirius.kernel.di.GlobalContext;
import sirius.kernel.di.std.Part;
import sirius.kernel.di.std.Register;
import sirius.kernel.health.HandledException;
import sirius.tagliatelle.macros.Macro;
import sirius.tagliatelle.tags.TagHandlerFactory;
import sirius.web.controller.Controller;
import sirius.web.controller.Routed;
import sirius.web.http.WebContext;

import java.util.Collection;
import java.util.List;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 * Provides a small helper to provide infos about available Tagliatelle tags.
 */
@Register
public class TagliatelleController implements Controller {

    @Part
    private Tagliatelle tagliatelle;

    @Part
    private GlobalContext context;

    private MultiMap<String, String> taglibTags;

    @Override
    public void onError(WebContext ctx, HandledException error) {
        ctx.respondWith().error(HttpResponseStatus.INTERNAL_SERVER_ERROR, error);
    }

    private MultiMap<String, String> getTagLibTags() {
        if (taglibTags == null) {

            MultiMap<String, String> result = MultiMap.createOrdered();
            Sirius.getClasspath()
                  .find(Pattern.compile("(default/)?taglib/([a-z]+)/(.*).html.pasta"))
                  .forEach(m -> result.put(m.group(2), m.group(3)));
            taglibTags = result;
        }

        return taglibTags;
    }

    private List<Tuple<String, String>> getTagLibs(MultiMap<String, String> tagLibTags) {
        return tagLibTags.keySet()
                         .stream()
                         .map(name -> Tuple.create(name,
                                                   Sirius.getSettings()
                                                         .get("tagliatelle.taglib." + name)
                                                         .asString(name)))
                         .collect(Collectors.toList());
    }

    @Routed("/system/tags")
    public void overview(WebContext ctx) {
        Collection<Macro> macros = context.getParts(Macro.class);
        Collection<String> builtIns = context.getParts(TagHandlerFactory.class)
                                             .stream()
                                             .map(TagHandlerFactory::getName)
                                             .map(name -> name.substring(2))
                                             .collect(Collectors.toList());
        List<Tuple<String, Class<?>>> globals = tagliatelle.getGlobalVariables();
        MultiMap<String, String> tagLibTags = getTagLibTags();
        List<Tuple<String, String>> tagLibs = getTagLibs(tagLibTags);

        ctx.respondWith().template("templates/system/tags.html.pasta", macros, globals, builtIns, tagLibs, tagLibTags);
    }

    @Routed("/system/tag/:1/:2")
    public void tagInfo(WebContext ctx, String tagLib, String tag) throws Exception {
        if ("i".equals(tagLib)) {
            TagHandlerFactory handler = context.findPart("i:" + tag, TagHandlerFactory.class);

            ctx.respondWith()
               .template("templates/system/tag.html.pasta",
                         tagLib,
                         tag,
                         handler.reportArguments(),
                         handler.getDescription(),
                         null);
        } else {
            Template template = tagliatelle.resolve("/taglib/" + tagLib + "/" + tag + ".html.pasta")
                                           .orElseThrow(() -> new IllegalArgumentException(tag));
            ctx.respondWith()
               .template("templates/system/tag.html.pasta",
                         tagLib,
                         tag,
                         template.getArguments(),
                         template.getPragma("description").asString(),
                         template.getResource().getContentAsString());
        }
    }
}
