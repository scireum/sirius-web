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
import sirius.tagliatelle.emitter.PushLocalEmitter;
import sirius.tagliatelle.expression.Expression;

import javax.annotation.Nonnull;

/**
 * Handles <tt>i:local</tt> which defines a local variable.
 */
public class LocalTag extends TagHandler {

    @Register
    public static class Factory implements TagHandlerFactory {

        @Nonnull
        @Override
        public String getName() {
            return "i:local";
        }

        @Override
        public TagHandler createHandler() {
            return new LocalTag();
        }
    }

    @Override
    public void apply(CompositeEmitter targetBlock) {
        String name = getConstantAttribute("name").asString();
        Expression value = getAttribute("value");
        if (value != null) {
            int variable = getCompilationContext().push(getStartOfTag(), name, value.getType());
            targetBlock.addChild(new PushLocalEmitter(startOfTag, variable, value));
        } else {
            compilationContext.error(startOfTag, "The attribute value is required.");
        }
    }

    @Override
    public Class<?> getExpectedAttributeType(String name) {
        if ("name".equals(name)) {
            return String.class;
        }

        if ("value".equals(name)) {
            return Expression.class;
        }

        return super.getExpectedAttributeType(name);
    }
}
