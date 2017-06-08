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
import sirius.tagliatelle.Engine;
import sirius.tagliatelle.Template;
import sirius.tagliatelle.emitter.CompositeEmitter;
import sirius.web.templates.Templates;

import javax.annotation.Nonnull;

/**
 * Handles <tt>i:extensions</tt> which invokes all extensions with the given name.
 */
public class TagExtensions extends TagInvoke {

    @Register
    public static class Factory implements TagHandlerFactory {

        @Nonnull
        @Override
        public String getName() {
            return "i:extensions";
        }

        @Override
        public TagHandler createHandler() {
            return new TagExtensions();
        }
    }

    @Part
    private static Templates templates;

    @Part
    private static Engine engine;

    @Override
    public void apply(CompositeEmitter targeBlock) {
        String name = getConstantAttribute("name").asString();
        for (String extension : templates.getExtensions(name)) {
            Template template = resolveTemplate(extension);

            if (template != null) {
                invokeTemplate(template, targeBlock);
            }
        }
    }

    @Override
    public Class<?> getExpectedAttributeType(String name) {
        if ("name".equals(name)) {
            return String.class;
        }

        if (ATTR_INLINE.equals(name)) {
            return boolean.class;
        }

        return super.getExpectedAttributeType(name);
    }
}
