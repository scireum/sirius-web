/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.tags;

import sirius.kernel.di.std.Register;
import sirius.tagliatelle.emitter.CompositeEmitter;
import sirius.tagliatelle.emitter.ConditionalEmitter;

import javax.annotation.Nonnull;

/**
 * Handles <tt>i:if</tt> which emits its body if a condition is met.
 */
public class IfTag extends TagHandler {
    @Register
    public static class Factory implements TagHandlerFactory {

        @Nonnull
        @Override
        public String getName() {
            return "i:if";
        }

        @Override
        public TagHandler createHandler() {
            return new IfTag();
        }
    }

    @Override
    public void apply(CompositeEmitter targetBlock) {
        ConditionalEmitter result = new ConditionalEmitter(getStartOfTag());
        result.setConditionExpression(getAttribute("test"));
        result.setWhenTrue(getBlock("body"));
        result.setWhenFalse(getBlock("else"));
        targetBlock.addChild(result);
    }

    @Override
    public Class<?> getExpectedAttributeType(String name) {
        if ("test".equals(name)) {
            return boolean.class;
        }

        return super.getExpectedAttributeType(name);
    }
}
