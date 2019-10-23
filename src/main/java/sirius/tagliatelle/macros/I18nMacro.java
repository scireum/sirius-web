/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.macros;

import parsii.tokenizer.Position;
import sirius.kernel.commons.Strings;
import sirius.kernel.di.std.Register;
import sirius.kernel.nls.NLS;
import sirius.tagliatelle.Tagliatelle;
import sirius.tagliatelle.compiler.CompilationContext;
import sirius.tagliatelle.expression.ConstantString;
import sirius.tagliatelle.expression.Expression;
import sirius.tagliatelle.rendering.LocalRenderContext;

import javax.annotation.Nonnull;
import java.util.List;

/**
 * Represents <tt>i18n(String)</tt> which is essentially a call to {@link NLS#get(String)}.
 */
@Register
public class I18nMacro implements Macro {

    @Override
    public Class<?> getType() {
        return String.class;
    }

    @Override
    public void verifyArguments(CompilationContext context, Position pos, List<Expression> args) {
        if (args.isEmpty()) {
            throw new IllegalArgumentException("Expected at least one String as argument.");
        }

        if (args.size() > 2) {
            throw new IllegalArgumentException("Expected at most two arguments.");
        }

        if (args.size() == 1 && !Tagliatelle.isAssignableTo(args.get(0).getType(), String.class)) {
            throw new IllegalArgumentException("Expected a single String as argument.");
        }

        if (args.size() == 2 && (!Tagliatelle.isAssignableTo(args.get(0).getType(), String.class)
                                 || !Tagliatelle.isAssignableTo(args.get(1).getType(), int.class))) {
            throw new IllegalArgumentException("Expected a String as first and an int as second argument.");
        }

        Expression expression = args.get(0);
        if (expression instanceof ConstantString) {
            String key = (String) expression.eval(null);
            if (Strings.isFilled(key) && NLS.getTranslationEngine()
                                            .getEntriesStartingWith(key)
                                            .noneMatch(entry -> key.equals(entry.getKey()))) {
                context.warning(pos, "No translation found for key: %s", key);
            }
        }
    }

    @Override
    public boolean isConstant(Expression[] args) {
        // An i18n macro is inherently not constant unless it is invoked for an empty string
        return (args.length == 1 || args.length == 2) && args[0].isConstant() && Strings.isEmpty(args[0].eval(null));
    }

    @Override
    public Object eval(LocalRenderContext ctx, Expression[] args) {
        String key = (String) args[0].eval(ctx);

        if (Strings.isFilled(key)) {
            if (args.length == 1) {
                return NLS.get(key);
            } else if (args.length == 2) {
                int numeric = (int) args[1].eval(ctx);
                return NLS.get(key, numeric);
            }
        }

        return "";
    }

    @Nonnull
    @Override
    public String getName() {
        return "i18n";
    }

    @Override
    public String getDescription() {
        return "Returns the translation for the given key in the currently active language.";
    }
}
