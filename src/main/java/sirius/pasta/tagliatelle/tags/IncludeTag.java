/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.tagliatelle.tags;

import sirius.kernel.di.std.Part;
import sirius.kernel.di.std.Register;
import sirius.pasta.tagliatelle.TemplateArgument;
import sirius.pasta.tagliatelle.emitter.CompositeEmitter;
import sirius.pasta.tagliatelle.emitter.ConstantEmitter;
import sirius.web.resources.Resource;
import sirius.web.resources.Resources;

import javax.annotation.Nonnull;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

/**
 * Handles <tt>i:include</tt> which includes the contents of the given resource without any processing.
 */
public class IncludeTag extends TagHandler {

    private static final String ATTR_NAME = "name";

    @Part
    private static Resources resources;

    /**
     * Creates new tags of the given type (name).
     */
    @Register
    public static class Factory implements TagHandlerFactory {

        @Nonnull
        @Override
        public String getName() {
            return "i:include";
        }

        @Override
        public TagHandler createHandler() {
            return new IncludeTag();
        }

        @Override
        public List<TemplateArgument> reportArguments() {
            return Collections.singletonList(new TemplateArgument(String.class,
                                                                  "name",
                                                                  "Contains the path of the resource to include."));
        }

        @Override
        public String getDescription() {
            return "Includes the contents of a template without any processing.";
        }
    }

    @Override
    public void apply(CompositeEmitter targetBlock) {
        String resourcePath = getConstantAttribute(ATTR_NAME).asString();
        if (!resourcePath.startsWith("/assets") && !resourcePath.startsWith("assets/")) {
            throw new IllegalArgumentException("For security reasons only assets can be included. Invalid path: "
                                               + resourcePath);
        }

        Optional<Resource> resource = resources.resolve(resourcePath);
        if (!resource.isPresent()) {
            getCompilationContext().error(getStartOfTag(), "Cannot find the resource: %s", resourcePath);
            return;
        }

        targetBlock.addChild(new ConstantEmitter(getStartOfTag()).append(resource.get().getContentAsString()));
    }

    @Override
    public Class<?> getExpectedAttributeType(String name) {
        if (ATTR_NAME.equals(name)) {
            return String.class;
        }

        return super.getExpectedAttributeType(name);
    }
}
