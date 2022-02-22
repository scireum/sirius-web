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
import sirius.pasta.noodle.sandbox.PublicApi;

import javax.annotation.Nonnull;
import java.time.LocalDateTime;
import java.util.List;

/**
 * Checks whether a given date is before another given date or now.
 */
@Register
@PublicApi
public class IsBeforeMacro extends DateComparingBaseMacro {

    @Override
    protected boolean compare(LocalDateTime firstDate, LocalDateTime secondDate) {
        return firstDate.isBefore(secondDate);
    }

    @Override
    public String getDescription() {
        return "Compares if the first given date / date with time is before the second given date / "
               + "date with time, or the current date / date with time if no second argument was given.";
    }

    @Nonnull
    @Override
    public String getName() {
        return "isBefore";
    }

    @Override
    public boolean isConstant(CompilationContext context, List<Node> args) {
        return true;
    }

}
