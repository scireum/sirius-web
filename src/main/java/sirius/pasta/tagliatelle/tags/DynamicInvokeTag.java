/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.tagliatelle.tags;

import sirius.kernel.di.std.Register;
import sirius.pasta.noodle.Callable;
import sirius.pasta.tagliatelle.TemplateArgument;
import sirius.pasta.tagliatelle.emitter.CompositeEmitter;
import sirius.pasta.tagliatelle.emitter.DynamicInvokeTemplateEmitter;

import javax.annotation.Nonnull;
import java.util.Collections;
import java.util.List;

/**
 * Handles <tt>i:dynamicInvoke</tt> which invokes or inlines a given template.
 */
public class DynamicInvokeTag extends TagHandler {

    private static final String ATTR_TEMPLATE = "template";

    /**
     * Creates new tags of the given type (name).
     */
    @Register
    public static class Factory implements TagHandlerFactory {

        @Nonnull
        @Override
        public String getName() {
            return "i:dynamicInvoke";
        }

        @Override
        public TagHandler createHandler() {
            return new DynamicInvokeTag();
        }

        @Override
        public List<TemplateArgument> reportArguments() {
            return Collections.singletonList(new TemplateArgument(String.class,
                                                                  ATTR_TEMPLATE,
                                                                  "Contains the path of the template to render."));
        }

        @Override
        public String getDescription() {
            return "Invokes a dynamically selected template."
                   + " Note that all template arguments also have to be passed as tag attributes.";
        }
    }

    @Override
    public void apply(CompositeEmitter targetBlock) {
        targetBlock.addChild(new DynamicInvokeTemplateEmitter(startOfTag,
                                                              getAttribute(ATTR_TEMPLATE),
                                                              attributes,
                                                              blocks));
    }

    @Override
    public Class<?> getExpectedAttributeType(String name) {
        // Accept anything, we don't know yet what to expect at runtime....
        return Callable.class;
    }
}
