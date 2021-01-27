/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.security;

import javax.annotation.Nonnull;

public class FactoryExampleHelper {

    public static class Factory implements HelperFactory<FactoryExampleHelper> {

        @Nonnull
        @Override
        public Class<FactoryExampleHelper> getHelperType() {
            return FactoryExampleHelper.class;
        }

        @Nonnull
        @Override
        public FactoryExampleHelper make(@Nonnull ScopeInfo scope) {
            return new FactoryExampleHelper();
        }
    }

}
