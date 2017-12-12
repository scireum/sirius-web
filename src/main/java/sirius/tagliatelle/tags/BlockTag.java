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
import sirius.tagliatelle.TemplateArgument;
import sirius.tagliatelle.emitter.CompositeEmitter;
import sirius.tagliatelle.emitter.ConstantEmitter;
import sirius.tagliatelle.emitter.Emitter;
import sirius.tagliatelle.emitter.ExtraBlockEmitter;

import javax.annotation.Nonnull;
import java.util.Collections;
import java.util.List;

/**
 * Handles <tt>i:block</tt> which specifies a template section passed into a tag invocation.
 */
public class BlockTag extends TagHandler {

    private static final String PARAM_NAME = "name";
    public static final String BLOCK_BODY = "body";

    /**
     * Creates new tags of the given type (name).
     */
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

        @Override
        public List<TemplateArgument> reportArguments() {
            return Collections.singletonList(new TemplateArgument(String.class,
                                                                  PARAM_NAME,
                                                                  "Contains the name of the provided block"));
        }

        @Override
        public String getDescription() {
            return "Declares a block which is passed within a template or tag invocation.";
        }
    }

    @Override
    public void apply(CompositeEmitter targetBlock) {
        String name = getConstantAttribute(PARAM_NAME).asString();
        if (Strings.isEmpty(name)) {
            getCompilationContext().error(getStartOfTag(), "The attribute name of i:block must be filled.", name);
            return;
        }
        if (getParentHandler() != null) {
            Emitter body = getBlock(BLOCK_BODY);
            if (body != null) {
                getParentHandler().addBlock(name, body);
            } else {
                getParentHandler().addBlock(name, ConstantEmitter.EMPTY);
            }
        } else {
            Emitter body = getBlock(BLOCK_BODY);
            if (body != null) {
                targetBlock.addChild(new ExtraBlockEmitter(name, body));
            }
        }
    }

    @Override
    public Class<?> getExpectedAttributeType(String name) {
        if (PARAM_NAME.equals(name)) {
            return String.class;
        }

        return super.getExpectedAttributeType(name);
    }
}
