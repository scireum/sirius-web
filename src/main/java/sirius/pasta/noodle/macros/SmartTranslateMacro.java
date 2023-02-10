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
 * Represents <tt>smartTranslate(String)</tt> which is essentially a call to {@link NLS#smartGet(String)}}.
 */
@Register
@NoodleSandbox(NoodleSandbox.Accessibility.GRANTED)
public class SmartTranslateMacro extends BasicMacro {
    @Override
    public Class<?> getType() {
        return String.class;
    }

    @Override
    public void verify(CompilationContext context, Position position, List<Node> args) {
        if (args.size() != 1 || !CompilationContext.isAssignableTo(args.get(0).getType(), String.class)) {
            throw new IllegalArgumentException("Expected a single String as argument.");
        }

        Node expression = args.get(0);
        if (expression.isConstant()) {
            String key = (String) expression.getConstantValue();
            if (key == null || !key.startsWith("$")) {
                return;
            }
            String effectiveKey = key.substring(1);
            if (Strings.isFilled(effectiveKey) && NLS.getTranslationEngine()
                                                     .getEntriesStartingWith(effectiveKey)
                                                     .noneMatch(entry -> effectiveKey.equals(entry.getKey()))) {
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
            return NLS.smartGet(key);
        } else {
            return "";
        }
    }

    @Override
    public boolean isConstant(CompilationContext context, List<Node> args) {
        // This macro is inherently not constant unless it is invoked for an empty string
        // or for a literal (which doesn't start with a $)
        if (args.size() != 1 || !args.get(0).isConstant()) {
            return false;
        }

        Object value = args.get(0).getConstantValue();
        return Strings.isEmpty(value) || !value.toString().startsWith("$");
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
