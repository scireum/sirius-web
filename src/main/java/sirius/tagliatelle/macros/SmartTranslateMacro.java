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
 * Represents <tt>smartTranslate(String)</tt> which is essentially a call to {@link NLS#smartGet(String)}}.
 */
@Register
public class SmartTranslateMacro implements Macro {
    @Override
    public Class<?> getType() {
        return String.class;
    }

    @Override
    public void verifyArguments(CompilationContext context, Position pos, List<Expression> args) {
        if (args.size() != 1 || !Tagliatelle.isAssignableTo(args.get(0).getType(), String.class)) {
            throw new IllegalArgumentException("Expected a single String as argument.");
        }

        Expression expression = args.get(0);
        if (expression instanceof ConstantString) {
            String key = (String) expression.eval(null);
            if (key == null || !key.startsWith("$")) {
                return;
            }
            String effectiveKey = key.substring(1);
            if (Strings.isFilled(effectiveKey) && NLS.getTranslationEngine()
                                                     .getEntriesStartingWith(effectiveKey)
                                                     .noneMatch(entry -> effectiveKey.equals(entry.getKey()))) {
                context.warning(pos, "No translation found for key: %s", key);
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
