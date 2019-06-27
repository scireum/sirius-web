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
import sirius.tagliatelle.emitter.CompositeEmitter;
import sirius.tagliatelle.emitter.ConditionalEmitter;
import sirius.tagliatelle.emitter.SwitchEmitter;

import javax.annotation.Nonnull;
import java.util.Collections;
import java.util.List;

/**
 * Handles <tt>i:if</tt> which emits its body if a condition is met.
 */
public class SwitchTag extends TagHandler {

    /**
     * Creates new tags of the given type (name).
     */
    @Register
    public static class Factory implements TagHandlerFactory {

        @Nonnull
        @Override
        public String getName() {
            return "i:switch";
        }

        @Override
        public TagHandler createHandler() {
            return new SwitchTag();
        }

        @Override
        public List<TemplateArgument> reportArguments() {
            return Collections.singletonList(new TemplateArgument(boolean.class,
                                                                  "test",
                                                                  "Contains the condition to check."));
        }

        @Override
        public String getDescription() {
            return "Emits its body only if the given condition evaluates to true. "
                   + "Use an inner i:else to emit if the condition is false.";
        }
    }

    @Override
    public void apply(CompositeEmitter targetBlock) {
        SwitchEmitter result = new SwitchEmitter(getStartOfTag());
        result.setSwitchExpression(getAttribute("test"));
        result.setBlocks(blocks);
        targetBlock.addChild(result);
    }

    @Override
    public Class<?> getExpectedAttributeType(String name) {
        if ("test".equals(name)) {
            return String.class;
        }

        return super.getExpectedAttributeType(name);
    }
}
