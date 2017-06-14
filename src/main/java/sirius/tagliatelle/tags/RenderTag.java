/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.tags;

import sirius.kernel.di.std.Register;
import sirius.tagliatelle.emitter.BlockEmitter;
import sirius.tagliatelle.emitter.CompositeEmitter;

import javax.annotation.Nonnull;

/**
 * Handles <tt>i:render</tt> which emits the block with the given name.
 */
public class RenderTag extends TagHandler {

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
    }

    @Override
    public void apply(CompositeEmitter targetBlock) {
        targetBlock.addChild(new BlockEmitter(getStartOfTag(),
                                              getConstantAttribute("name").asString(),
                                              getBlock("body")));
    }

    @Override
    public Class<?> getExpectedAttributeType(String name) {
        if ("name".equals(name)) {
            return String.class;
        }

        return super.getExpectedAttributeType(name);
    }
}
