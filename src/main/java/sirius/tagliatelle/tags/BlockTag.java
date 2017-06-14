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
import sirius.tagliatelle.emitter.CompositeEmitter;
import sirius.tagliatelle.emitter.ConstantEmitter;
import sirius.tagliatelle.emitter.Emitter;

import javax.annotation.Nonnull;

/**
 * Handles <tt>i:block</tt> which specifies a template section passed into a tag invocation.
 */
public class BlockTag extends TagHandler {

    @Register
    public static class Factory implements TagHandlerFactory {

        @Nonnull
        @Override
        public String getName() {
            return "i:block";
        }

        @Override
        public TagHandler createHandler() {
            return new BlockTag();
        }
    }

    @Override
    public void apply(CompositeEmitter targetBlock) {
        if (getParentHandler() != null) {

            String name = getConstantAttribute("name").asString();
            if (Strings.isEmpty(name)) {
                getCompilationContext().error(getStartOfTag(), "The attribute name of i:block must be filled.", name);
            } else {
                Emitter body = getBlock("body");
                if (body != null) {
                    getParentHandler().addBlock(name, body);
                } else {
                    getParentHandler().addBlock(name, ConstantEmitter.EMPTY);
                }
            }
        } else {
            getCompilationContext().error(getStartOfTag(), "Cannot define a block without a surrounding tag.");
        }
    }

    @Override
    public Class<?> getExpectedAttributeType(String name) {
        if ("name".equals(name)) {
            return String.class;
        }

        return super.getExpectedAttributeType(name);
    }
}
