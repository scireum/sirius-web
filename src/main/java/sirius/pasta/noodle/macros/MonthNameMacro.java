/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.macros;

import sirius.kernel.tokenizer.Position;
import sirius.kernel.di.std.Register;
import sirius.kernel.nls.NLS;
import sirius.pasta.noodle.Environment;
import sirius.pasta.noodle.compiler.CompilationContext;
import sirius.pasta.noodle.compiler.ir.Node;
import sirius.pasta.noodle.sandbox.NoodleSandbox;

import javax.annotation.Nonnull;
import java.util.List;

/**
 * Represents <tt>monthName(int)</tt> which is a call to {@link NLS#getMonthName(int)}.
 */
@Register
@NoodleSandbox(NoodleSandbox.Accessibility.GRANTED)
public class MonthNameMacro extends BasicMacro {
    @Override
    public Class<?> getType() {
        return String.class;
    }

    @Override
    public void verifyArguments(CompilationContext context, Position position, List<Class<?>> args) {
        if (args.size() != 1 || !CompilationContext.isAssignableTo(args.get(0), int.class)) {
            throw new IllegalArgumentException("Expected exactly one argument as an int.");
        }
    }

    @Override
    public Object invoke(Environment environment, Object[] args) {
        return NLS.getMonthName((Integer) args[0]);
    }

    @Override
    public boolean isConstant(CompilationContext context, List<Node> args) {
        return false;
    }

    @Override
    public String getDescription() {
        return "Generates the string representation of a month";
    }

    @Nonnull
    @Override
    public String getName() {
        return "monthName";
    }
}
