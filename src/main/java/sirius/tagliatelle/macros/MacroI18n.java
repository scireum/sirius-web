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
import sirius.kernel.nls.NLS;
import sirius.tagliatelle.expression.ConstantString;
import sirius.tagliatelle.expression.Expression;
import sirius.tagliatelle.rendering.LocalRenderContext;

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
        if (args.size() != 1 || !String.class.isAssignableFrom(args.get(0).getType())) {
            throw new IllegalArgumentException("Expected a single String as argument.");
        }

        if (Sirius.isDev()) {
            Expression exp = args.get(0);
            if (exp instanceof ConstantString) {
                String key = (String) exp.eval(null);
                if (NLS.getTranslationEngine().getTranslations(key).noneMatch(t -> key.equals(t.getKey()))) {
                    throw new IllegalArgumentException(Strings.apply("No translation found for key: %s", key));
                }
            }
        }
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
