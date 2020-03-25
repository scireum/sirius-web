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
import sirius.tagliatelle.emitter.Emitter;
import sirius.tagliatelle.emitter.ExtraBlockEmitter;

import javax.annotation.Nonnull;
import java.util.Collections;
import java.util.List;

/**
 * Permits to add an extra block to the global render context.
 * <p>
 * This is a kind of a special tag which permits that its body is added as extra block to the current global
 * render context. Normally all {@link BlockTag blocks} defined at the top-level of the root template
 * are added as extra block. However, using this block, an inner template (e.g. a tag being invoked) can
 * also supply data into an extra block.
 *
 * @see sirius.tagliatelle.rendering.GlobalRenderContext#storeExtraBlock(String, String)
 */
public class ExtraBlockTag extends TagHandler {

    private static final String PARAM_NAME = "name";
    private static final String BLOCK_BODY = "body";

    /**
     * Creates new tags of the given type (name).
     */
    @Register
    public static class Factory implements TagHandlerFactory {

        @Nonnull
        @Override
        public String getName() {
            return "i:extraBlock";
        }

        @Override
        public TagHandler createHandler() {
            return new ExtraBlockTag();
        }

        @Override
        public List<TemplateArgument> reportArguments() {
            return Collections.singletonList(new TemplateArgument(String.class,
                                                                  PARAM_NAME,
                                                                  "Contains the name of the provided block"));
        }

        @Override
        public String getDescription() {
            return "Declares an extra block which is directly made available to the GlobalRenderContext";
        }
    }

    @Override
    public void apply(CompositeEmitter targetBlock) {
        String name = getConstantAttribute(PARAM_NAME).asString();
        if (Strings.isEmpty(name)) {
            getCompilationContext().error(getStartOfTag(), "The attribute name of i:extraBlock must be filled.", name);
            return;
        }

        Emitter body = getBlock(BLOCK_BODY);
        if (body != null) {
            targetBlock.addChild(new ExtraBlockEmitter(name, body));
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
