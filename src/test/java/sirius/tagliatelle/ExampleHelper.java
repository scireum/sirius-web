/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle;

import sirius.kernel.di.std.Register;
import sirius.web.security.HelperFactory;
import sirius.web.security.ScopeInfo;

import javax.annotation.Nonnull;

public class ExampleHelper {

    @Register
    public static class ExampleHelperFactory implements HelperFactory<ExampleHelper> {

        @Nonnull
        @Override
        public Class<ExampleHelper> getHelperType() {
            return ExampleHelper.class;
        }

        @Nonnull
        @Override
        public String getName() {
            return "example";
        }

        @Nonnull
        @Override
        public ExampleHelper make(@Nonnull ScopeInfo scope) {
            return new ExampleHelper();
        }
    }

    public String getTestValue() {
        return "test";
    }

}
