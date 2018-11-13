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
import sirius.tagliatelle.Tagliatelle;
import sirius.tagliatelle.expression.ConstantString;
import sirius.tagliatelle.expression.Expression;
import sirius.tagliatelle.rendering.LocalRenderContext;

import javax.annotation.Nonnull;
import java.util.List;

/**
 * Represents <tt>smartTranslate(String)</tt> which is essentially a call to {@link NLS#smartGet(String)}}.
 */
@Register
public class SmartTranslateMacro implements Macro {
    @Override
    public Class<?> getType() {
        return String.class;
    }

    /**
     * Verifies the argument type.
     * <p>
     * If the i18n key is constant, it also ensures that a matching translation is present.
     *
     * @param args the expressions which will be passed in at runtime.
     */
    @Override
    public void verifyArguments(List<Expression> args) {
        if (args.size() != 1 || !Tagliatelle.isAssignableTo(args.get(0).getType(), String.class)) {
            throw new IllegalArgumentException("Expected a single String as argument.");
        }

        if (Sirius.isDev()) {
            Expression exp = args.get(0);
            if (exp instanceof ConstantString) {
                String key = (String) exp.eval(null);
                if (key == null || !key.startsWith("$")) {
                    return;
                }
                String effectiveKey = key.substring(1);
                if (Strings.isFilled(effectiveKey) && NLS.getTranslationEngine()
                                                         .getEntriesStartingWith(effectiveKey)
                                                         .noneMatch(t -> effectiveKey.equals(t.getKey()))) {
                    throw new IllegalArgumentException(Strings.apply("No translation found for key: %s", effectiveKey));
                }
            }
        }
    }

    @Override
    public boolean isConstant(Expression[] args) {
        // This macro is inherently not constant unless it is invoked for an empty string
        // or for a literal (which doesn't start with a $)
        if (args.length != 1 || !args[0].isConstant()) {
            return false;
        }

        Object value = args[0].eval(null);
        return Strings.isEmpty(value) || !value.toString().startsWith("$");
    }

    @Override
    public Object eval(LocalRenderContext ctx, Expression[] args) {
        String key = (String) args[0].eval(ctx);
        if (Strings.isFilled(key)) {
            return NLS.smartGet(key);
        } else {
            return "";
        }
    }

    @Nonnull
    @Override
    public String getName() {
        return "smartTranslate";
    }

    @Override
    public String getDescription() {
        return "Tries to translate the given string (in the currently active language) if it starts with a $. Otherwise the given string is returned.";
    }
}
