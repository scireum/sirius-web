/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.macros;

import sirius.kernel.di.std.Register;
import sirius.pasta.noodle.compiler.CompilationContext;
import sirius.pasta.noodle.compiler.ir.Node;
import sirius.pasta.noodle.sandbox.NoodleSandbox;

import javax.annotation.Nonnull;
import java.time.LocalDateTime;
import java.util.List;

/**
 * Checks whether a given date is after another given date or now.
 */
@Register
@NoodleSandbox(NoodleSandbox.Accessibility.GRANTED)
public class IsAfterMacro extends DateComparingBaseMacro {

    @Override
    protected boolean compare(LocalDateTime firstDate, LocalDateTime secondDate) {
        return firstDate.isAfter(secondDate);
    }

    @Override
    public String getDescription() {
        return "Compares if the first given date / date with time is after the second given date / "
               + "date with time or the curent date / date with time if no second argument was given.";
    }

    @Nonnull
    @Override
    public String getName() {
        return "isAfter";
    }

    @Override
    public boolean isConstant(CompilationContext context, List<Node> args) {
        return true;
    }

}
