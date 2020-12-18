/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.tagliatelle.tags;

import sirius.kernel.di.std.Register;
import sirius.pasta.tagliatelle.TemplateArgument;
import sirius.pasta.tagliatelle.emitter.CompositeEmitter;
import sirius.pasta.tagliatelle.emitter.SwitchEmitter;

import javax.annotation.Nonnull;
import java.util.Collections;
import java.util.List;

/**
 * Handles <tt>i:switch</tt> which can hold multiple blocks which are only rendered if their name matches an expression.
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
                    "Contains the switch expression to evaluate."));
        }

        @Override
        public String getDescription() {
            return "Emits the inner i:block only if their name matches the 'test' expression.";
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
