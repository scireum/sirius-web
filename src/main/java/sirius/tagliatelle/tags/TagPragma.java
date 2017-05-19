/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.tags;

import sirius.kernel.di.std.Register;
import sirius.tagliatelle.TagContext;

import javax.annotation.Nonnull;

/**
 * Created by aha on 12.05.17.
 */
public class TagPragma extends TagHandler {

    @Register
    public static class Factory implements TagHandlerFactory {

        @Nonnull
        @Override
        public String getName() {
            return "i:pragma";
        }

        @Override
        public TagHandler createHandler() {
            return new TagPragma();
        }
    }

    @Override
    public void apply(TagContext context) {
        context.getContext()
               .getTemplate()
               .addPragma(getConstantAttribute("name").asString(), getConstantAttribute("value").asString());
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
