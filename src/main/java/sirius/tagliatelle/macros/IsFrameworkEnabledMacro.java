/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.macros;

import sirius.kernel.Sirius;
import sirius.kernel.commons.Strings;
import sirius.kernel.di.std.Register;
import sirius.tagliatelle.expression.Expression;
import sirius.tagliatelle.rendering.LocalRenderContext;

import javax.annotation.Nonnull;
import java.util.List;

/**
 * Represents a call to {@link  Sirius#isFrameworkEnabled(String)}.
 */
@Register
public class IsFrameworkEnabledMacro implements Macro {

    @Override
    public Class<?> getType() {
        return boolean.class;
    }

    @Override
    public void verifyArguments(List<Expression> args) {
        if (args.size() != 1) {
            throw new IllegalArgumentException("Expected a single String as argument.");
        }
    }

    @Override
    public boolean isConstant(Expression[] args) {
        return true;
    }

    @Override
    public Object eval(LocalRenderContext ctx, Expression[] args) {
        String framework = (String) args[0].eval(ctx);
        return Strings.isEmpty(framework) || Sirius.isFrameworkEnabled(framework);
    }

    @Nonnull
    @Override
    public String getName() {
        return "isFrameworkEnabled";
    }

    @Override
    public String getDescription() {
        return "Returns true, if the given framework is enabled, false otherwise.";
    }
}
