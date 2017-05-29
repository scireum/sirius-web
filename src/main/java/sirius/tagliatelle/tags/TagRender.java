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

import javax.annotation.Nonnull;

/**
 * Created by aha on 12.05.17.
 */
public class TagRender extends TagHandler {

    @Register
    public static class Factory implements TagHandlerFactory {

        @Nonnull
        @Override
        public String getName() {
            return "i:render";
        }

        @Override
        public TagHandler createHandler() {
            return new TagRender();
        }
    }

    @Override
    public void apply(TagContext context) {
        context.getBlock().addChild(new BlockEmitter(context.getStartOfTag(), getConstantAttribute("name").asString(), getBlock("body")));
    }

    @Override
    public Class<?> getExpectedAttributeType(String name) {
        if ("name".equals(name)) {
            return String.class;
        }

        return super.getExpectedAttributeType(name);
    }
}
