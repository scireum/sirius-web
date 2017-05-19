/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.macros;

import sirius.kernel.di.std.Register;
import sirius.kernel.nls.NLS;
import sirius.tagliatelle.LocalRenderContext;
import sirius.tagliatelle.expression.Expression;

import javax.annotation.Nonnull;
import java.util.List;

/**
 * Created by aha on 16.05.17.
 */
@Register
public class MacroI18n implements Macro {
    @Override
    public Class<?> getType() {
        return String.class;
    }

    @Override
    public void verifyArguments(List<Expression> args) {
//if (args.size() != 1 || x)
        //TODO
    }

    @Override
    public boolean isConstant() {
        return false;
    }

    @Override
    public Object eval(LocalRenderContext ctx, Expression[] args) {
        return NLS.get((String) args[0].eval(ctx));
    }

    @Nonnull
    @Override
    public String getName() {
        return "i18n";
    }
}
