/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.macros;

import sirius.kernel.tokenizer.Position;
import sirius.kernel.commons.Strings;
import sirius.kernel.di.std.Register;
import sirius.kernel.nls.NLS;
import sirius.pasta.noodle.Environment;
import sirius.pasta.noodle.compiler.CompilationContext;
import sirius.pasta.noodle.compiler.ir.Node;
import sirius.pasta.noodle.sandbox.NoodleSandbox;

import javax.annotation.Nonnull;
import java.util.List;

/**
 * Represents <tt>i18n(String)</tt> which is essentially a call to {@link NLS#get(String)}.
 */
@Register
@NoodleSandbox(NoodleSandbox.Accessibility.GRANTED)
public class I18nMacro extends BasicMacro {

    @Override
    public Class<?> getType() {
        return String.class;
    }

    @Override
    public void verify(CompilationContext context, Position position, List<Node> args) {
        if (args.isEmpty()) {
            throw new IllegalArgumentException("Expected at least one String as argument.");
        }

        if (args.size() > 2) {
            throw new IllegalArgumentException("Expected at most two arguments.");
        }

        if (args.size() == 1 && !CompilationContext.isAssignableTo(args.get(0).getType(), String.class)) {
            throw new IllegalArgumentException("Expected a single String as argument.");
        }

        if (args.size() == 2 && (!CompilationContext.isAssignableTo(args.get(0).getType(), String.class)
                                 || !CompilationContext.isAssignableTo(args.get(1).getType(), int.class))) {
            throw new IllegalArgumentException("Expected a String as first and an int as second argument.");
        }

        Node expression = args.get(0);
        if (expression.isConstant()) {
            String key = (String) expression.getConstantValue();
            if (Strings.isFilled(key) && NLS.getTranslationEngine()
                                            .getEntriesStartingWith(key)
                                            .noneMatch(entry -> key.equals(entry.getKey()))) {
                context.warning(position, "No translation found for key: %s", key);
            }
        }
    }

    @Override
    protected void verifyArguments(CompilationContext compilationContext, Position position, List<Class<?>> args) {
        throw new UnsupportedOperationException("unreachable");
    }

    @Override
    public Object invoke(Environment environment, Object[] args) {
        String key = (String) args[0];

        if (Strings.isFilled(key)) {
            if (args.length == 1) {
                return NLS.get(key);
            } else if (args.length == 2) {
                int numeric = (int) args[1];
                return NLS.get(key, numeric);
            }
        }

        return "";
    }

    @Override
    public boolean isConstant(CompilationContext context, List<Node> args) {
        // An i18n macro is inherently not constant unless it is invoked for an empty string
        return (args.size() == 1 || args.size() == 2) && args.get(0).isConstant() && Strings.isEmpty(args.get(0)
                                                                                                         .getConstantValue());
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
