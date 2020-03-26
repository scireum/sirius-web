/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.tags;

import sirius.kernel.commons.Strings;
import sirius.kernel.di.std.Register;
import sirius.tagliatelle.TemplateArgument;
import sirius.tagliatelle.emitter.BlockEmitter;
import sirius.tagliatelle.emitter.CompositeEmitter;

import javax.annotation.Nonnull;
import java.util.Collections;
import java.util.List;

/**
 * Handles <tt>i:render</tt> which emits the block with the given name.
 */
public class RenderTag extends TagHandler {

    protected static final String PARAM_NAME = "name";

    /**
     * Creates new tags of the given type (name).
     */
    @Register
    public static class Factory implements TagHandlerFactory {

        @Nonnull
        @Override
        public String getName() {
            return "i:render";
        }

        @Override
        public TagHandler createHandler() {
            return new RenderTag();
        }

        @Override
        public List<TemplateArgument> reportArguments() {
            return Collections.singletonList(new TemplateArgument(String.class,
                                                                  PARAM_NAME,
                                                                  "Contains the name of the block to render"));
        }

        @Override
        public String getDescription() {
            return "Renders a block which was provided by the caller.";
        }
    }

    @Override
    public void apply(CompositeEmitter targetBlock) {
        String name = getConstantAttribute(PARAM_NAME).asString();
        if (Strings.isEmpty(name)) {
            getCompilationContext().error(getStartOfTag(),
                                          "The attribute name of i:render must be filled."
                                          + " Use 'body' to refer to the default block defined when invoking a tag.",
                                          name);
            return;
        }

        targetBlock.addChild(new BlockEmitter(getStartOfTag(),
                                              getConstantAttribute(PARAM_NAME).asString(),
                                              getBlock("body")));
    }

    @Override
    public Class<?> getExpectedAttributeType(String name) {
        if (PARAM_NAME.equals(name)) {
            return String.class;
        }

        return super.getExpectedAttributeType(name);
    }
}
