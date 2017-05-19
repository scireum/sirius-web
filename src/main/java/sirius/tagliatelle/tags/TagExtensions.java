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
import sirius.tagliatelle.TagContext;
import sirius.web.templates.Templates;

import javax.annotation.Nonnull;

/**
 * Created by aha on 12.05.17.
 */
public class TagExtensions extends TagHandler {

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
    public void apply(TagContext context) {
        String name =getConstantAttribute("name").asString();
        for(String extension : templates.getExtensions(name)) {
//            engine.resolve(extension)
        }
//        context.getContext()
//               .getTemplate()
//               .addPragma(, getConstantAttribute("value").asString());
    }

    @Override
    public Class<?> getExpectedAttributeType(String name) {
        if ("name".equals(name)) {
            return String.class;
        }

        if ("value".equals(name)) {
            return String.class;
        }

        return super.getExpectedAttributeType(name);
    }
}
