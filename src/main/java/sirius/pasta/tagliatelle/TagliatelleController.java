/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.tagliatelle;

import io.netty.handler.codec.http.HttpResponseStatus;
import sirius.kernel.di.GlobalContext;
import sirius.kernel.di.std.Part;
import sirius.kernel.di.std.Register;
import sirius.kernel.health.HandledException;
import sirius.pasta.noodle.compiler.CompileException;
import sirius.pasta.noodle.macros.Macro;
import sirius.pasta.tagliatelle.tags.TagHandlerFactory;
import sirius.web.controller.BasicController;
import sirius.web.controller.Routed;
import sirius.web.http.WebContext;
import sirius.web.security.Permission;

import java.util.Collection;
import java.util.Comparator;

/**
 * Provides a small helper to provide infos about available Tagliatelle tags.
 */
@Register
public class TagliatelleController extends BasicController {

    /**
     * Describes the permission required to access the Tagliatelle infos
     */
    public static final String PERMISSION_SYSTEM_TAGS = "permission-system-tags";

    /**
     * Describes the permission required to access the Tagliatelle state
     */
    public static final String PERMISSION_SYSTEM_TAGS_STATE = "permission-system-tags-state";

    @Part
    private Tagliatelle tagliatelle;

    @Part
    private GlobalContext context;

    @Override
    public void onError(WebContext webContext, HandledException error) {
        webContext.respondWith().error(HttpResponseStatus.INTERNAL_SERVER_ERROR, error);
    }

    /**
     * Renders a list of all known tags.
     *
     * @param webContext the request to process
     */
    @Routed("/system/tags")
    @Permission(PERMISSION_SYSTEM_TAGS)
    public void overview(WebContext webContext) {
        Collection<Macro> macros =
                context.getParts(Macro.class).stream().sorted(Comparator.comparing(Macro::getName)).toList();
        Collection<String> builtIns = context.getParts(TagHandlerFactory.class)
                                             .stream()
                                             .map(TagHandlerFactory::getName)
                                             .map(name -> name.substring(2))
                                             .sorted()
                                             .toList();

        webContext.respondWith()
                  .template("/templates/system/tags.html.pasta",
                            macros,
                            builtIns,
                            tagliatelle.getTagLibs(),
                            tagliatelle.getTagLibTags());
    }

    /**
     * Renders details for the given tag.
     *
     * @param tagLib     the library which contains the tag
     * @param tag        the name of the tag
     * @param webContext the request to process
     * @throws CompileException in case of an error while rendering the tag template
     */
    @Routed("/system/tag/:1/:2")
    @Permission(PERMISSION_SYSTEM_TAGS)
    public void tagInfo(WebContext webContext, String tagLib, String tag) throws CompileException {
        if ("i".equals(tagLib)) {
            TagHandlerFactory handler = context.findPart("i:" + tag, TagHandlerFactory.class);

            webContext.respondWith()
                      .template("/templates/system/tag.html.pasta",
                                tagLib,
                                tag,
                                handler.reportArguments(),
                                handler.getDescription(),
                                null);
        } else {
            Template template = tagliatelle.resolve("/taglib/" + tagLib + "/" + tag + ".html.pasta")
                                           .orElseThrow(() -> new IllegalArgumentException(tag));
            webContext.respondWith()
                      .template("/templates/system/tag.html.pasta",
                                tagLib,
                                tag,
                                template.getArguments(),
                                template.getPragma("description").asString(),
                                template.getResource().getContentAsString());
        }
    }

    /**
     * Provides some statistics and metrics for the internals of tagliatelle.
     *
     * @param webContext the request being handled
     */
    @Routed("/system/tags/state")
    @Permission(PERMISSION_SYSTEM_TAGS_STATE)
    public void tagState(WebContext webContext) {
        if (webContext.hasParameter("reset")) {
            tagliatelle.getCompiledTemplates().forEach(Template::resetAverageRenderTime);
        }
        webContext.respondWith()
                  .template("/templates/system/tags-state.html.pasta", tagliatelle.getCompiledTemplates());
    }
}
