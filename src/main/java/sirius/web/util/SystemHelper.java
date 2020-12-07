/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.util;

import sirius.kernel.Setup;
import sirius.kernel.Sirius;
import sirius.kernel.di.std.Register;
import sirius.web.security.HelperFactory;
import sirius.web.security.ScopeInfo;

import javax.annotation.Nonnull;

/**
 * Provides static system functionality and system environment detection based on {@link sirius.kernel.Setup.Mode} as
 * helper for in-template usage.
 */
public class SystemHelper {

    private static final String SYSTEM_HELPER = "SystemHelper";

    @Register
    public static class SystemHelperFactory implements HelperFactory<SystemHelper> {

        @Override
        public Class<SystemHelper> getHelperType() {
            return SystemHelper.class;
        }

        @Nonnull
        @Override
        public String getName() {
            return SYSTEM_HELPER;
        }

        @Override
        public SystemHelper make(ScopeInfo scopeInfo) {
            return new SystemHelper();
        }
    }

    /**
     * Returns the value of the given environment variable.
     *
     * @param varName the environment variable to look up
     * @return the value of the environment variable
     */
    public String getEnvironmentVariable(String varName) {
        return System.getenv(varName);
    }

    /**
     * Returns the current {@link sirius.kernel.Setup.Mode} of the system.
     *
     * @return the current {@link sirius.kernel.Setup.Mode} of the system.
     */
    public Setup.Mode getCurrentSystemEnvironment() {
        return Sirius.getSetup().getMode();
    }

    /**
     * Determines if the current System is a productive system.
     *
     * @return <tt>true</tt> if this is a productive system
     */
    public boolean isProductiveSystem() {
        return getCurrentSystemEnvironment() == Setup.Mode.PROD;
    }
}
