/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.security;

import sirius.kernel.commons.Explain;
import sirius.kernel.di.std.Register;
import sirius.kernel.health.Exceptions;

import javax.annotation.Nonnull;

public class AnotherExampleHelper {

    @Register
    public static class AnotherExampleHelperFactory implements HelperFactory<AnotherExampleHelper> {

        @Nonnull
        @Override
        public Class<AnotherExampleHelper> getHelperType() {
            return AnotherExampleHelper.class;
        }

        @Nonnull
        @Override
        public String getName() {
            return "example2";
        }

        @Nonnull
        @Override
        @SuppressWarnings("squid:S2925")
        @Explain("We need this delay here to test loading friend helpers.")
        public AnotherExampleHelper make(@Nonnull ScopeInfo scope) {
            try {
                Thread.sleep(100);
            } catch (InterruptedException e) {
                throw Exceptions.handle(e);
            }

            return new AnotherExampleHelper();
        }
    }

    public String getTestValue() {
        return "test";
    }
}
