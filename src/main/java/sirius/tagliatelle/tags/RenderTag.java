/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.tags;

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

    public static final String PARAM_NAME = "name";

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
                                                                  "Contains the name of the block to render",
                                                                  null));
        }

        @Override
        public String getDescription() {
            return "Renders a block which was provided by the caller.";
        }
    }

    @Override
    public void apply(CompositeEmitter targetBlock) {
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
