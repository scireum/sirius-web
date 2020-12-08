/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.macros;

import sirius.kernel.Sirius;
import sirius.kernel.di.std.Register;
import sirius.tagliatelle.expression.Expression;
import sirius.tagliatelle.rendering.LocalRenderContext;

import javax.annotation.Nonnull;

/**
 * This macro determines if the current system environment is in staging {@link sirius.kernel.Setup.Mode}.
 */
@Register
public class IsStagingMacro extends EnvironmentModeMacro {

    @Override
    public Object eval(LocalRenderContext ctx, Expression[] args) {
        return Sirius.isStaging();
    }

    @Override
    public String getDescription() {
        return "Returns true if system environment in staging mode, false otherwise.";
    }

    @Nonnull
    @Override
    public String getName() {
        return "isStaging";
    }
}
