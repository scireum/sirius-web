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

import javax.annotation.Nonnull;
import java.util.Collections;
import java.util.List;

/**
 * Handles <tt>i:if</tt> which emits its body if a condition is met.
 */
public class IfTag extends TagHandler {

    private static final String EMPTY_STRING = "";

    /**
     * Creates new tags of the given type (name).
     */
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

    private int localIndex;

    @Override
    public void beforeBody() {
        localIndex = getCompilationContext().push(getStartOfTag(), EMPTY_STRING, String.class);
    }

    @Override
    public void apply(CompositeEmitter targetBlock) {
        ConditionalEmitter result = new ConditionalEmitter(getStartOfTag());
        result.setConditionExpression(getAttribute("test"));
        result.setWhenTrue(getBlock("body"));
        result.setWhenFalse(getBlock("else"));
        targetBlock.addChild(result);
        getCompilationContext().tryPopUntil(getStartOfTag(), localIndex);
    }

    @Override
    public Class<?> getExpectedAttributeType(String name) {
        if ("test".equals(name)) {
            return boolean.class;
        }

        return super.getExpectedAttributeType(name);
    }

    /**
     * Clears all local variables created by this if-tag from the stack. This is mainly needed, as <i:if></i:if>
     * can contain a <i:else></i:else> and we have to clear the locals before entering the else-block.
     */
    public void clearLocalsFromStack() {
        getCompilationContext().popUntil(getStartOfTag(), localIndex);
    }
}
