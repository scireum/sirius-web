/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.macros;

import sirius.kernel.Sirius;
import sirius.kernel.di.std.Register;
import sirius.pasta.noodle.Environment;

import javax.annotation.Nonnull;

/**
 * This macro determines if the current system environment is in production {@link sirius.kernel.Setup.Mode}.
 */
@Register
public class IsProductionMacro extends EnvironmentModeMacro {

    @Override
    public Object invoke(Environment environment, Object[] args) {
        return Sirius.isProd();
    }

    @Override
    public String getDescription() {
        return "Returns true if system environment in production mode, false otherwise.";
    }

    @Nonnull
    @Override
    public String getName() {
        return "isProduction";
    }
}
