/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.tags;

import sirius.kernel.di.std.Part;
import sirius.kernel.di.std.Register;
import sirius.tagliatelle.Tagliatelle;
import sirius.tagliatelle.Template;
import sirius.tagliatelle.TemplateArgument;
import sirius.tagliatelle.emitter.CompositeEmitter;
import sirius.tagliatelle.expression.Expression;
import sirius.web.templates.Templates;

import javax.annotation.Nonnull;
import java.util.Collections;
import java.util.List;

/**
 * Handles <tt>i:extensions</tt> which invokes all extensions with the given name.
 */
public class ExtensionsTag extends InvokeTag {

    private static final String ATTR_NAME = "name";

    /**
     * Creates new tags of the given type (name).
     */
    @Register
    public static class Factory implements TagHandlerFactory {

        @Nonnull
        @Override
        public String getName() {
            return "i:extensions";
        }

        @Override
        public TagHandler createHandler() {
            return new ExtensionsTag();
        }

        @Override
        public List<TemplateArgument> reportArguments() {
            return Collections.singletonList(new TemplateArgument(String.class,
                                                                  ATTR_NAME,
                                                                  "Contains the name used to fetch all known extensions.",
                                                                  null));
        }

        @Override
        public String getDescription() {
            return "Invokes all templates which are provided for a given extension point.";
        }
    }

    @Part
    private static Templates templates;

    @Part
    private static Tagliatelle engine;

    @Override
    public void apply(CompositeEmitter targetBlock) {
        String name = getConstantAttribute(ATTR_NAME).asString();
        for (String extension : templates.getExtensions(name)) {
            Template template = resolveTemplate(extension);

            if (template != null) {
                invokeTemplate(template, targetBlock);
            }
        }
    }

    @Override
    public Class<?> getExpectedAttributeType(String name) {
        if (ATTR_NAME.equals(name)) {
            return String.class;
        }

        if (ATTR_INLINE.equals(name)) {
            return boolean.class;
        }

        return Expression.class;
    }
}
