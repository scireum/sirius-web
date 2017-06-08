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
import sirius.tagliatelle.emitter.LoopEmitter;
import sirius.tagliatelle.expression.Expression;

import javax.annotation.Nonnull;

/**
 * Handles <tt>i:for</tt> which emits its body for each item in an {@link Iterable}.
 */
public class TagFor extends TagHandler {

    @Register
    public static class Factory implements TagHandlerFactory {

        @Nonnull
        @Override
        public String getName() {
            return "i:for";
        }

        @Override
        public TagHandler createHandler() {
            return new TagFor();
        }
    }

    private int localIndex;

    @Override
    public void beforeBody() {
        Class<?> type = getCompilationContext().resolveClass(getStartOfTag(), getConstantAttribute("type").asString());
        localIndex = getCompilationContext().push(getStartOfTag(), getConstantAttribute("var").asString(), type);
    }

    @Override
    public void apply(CompositeEmitter targetBlock) {
        LoopEmitter result = new LoopEmitter(getStartOfTag());
        result.setIterableExpression(getAttribute("items"));
        result.setLoop(getBlock("body"));
        result.setLocalIndex(localIndex);
        result.verify(getCompilationContext());
        targetBlock.addChild(result);
        getCompilationContext().pop(getStartOfTag());
    }

    @Override
    public Class<?> getExpectedAttributeType(String name) {
        if ("items".equals(name)) {
            return Expression.class;
        }
        if ("type".equals(name)) {
            return String.class;
        }
        if ("var".equals(name)) {
            return String.class;
        }

        return super.getExpectedAttributeType(name);
    }
}
