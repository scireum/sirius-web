/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.macros;

import sirius.kernel.di.std.Register;
import sirius.pasta.noodle.sandbox.PublicAPI;

import javax.annotation.Nonnull;
import java.time.LocalDateTime;

/**
 * Macro for comparing it a given date is before another given date or now.
 */
@Register
@PublicAPI
public class IsBeforeMacro extends DateComparingBaseMacro {

    @Override
    protected boolean compare(LocalDateTime firstDate, LocalDateTime secondDate) {
        return firstDate.isBefore(secondDate);
    }

    @Override
    public String getDescription() {
        return "Compares if the first given date / date with time is before the second given date / "
               + "date with time or the curent date / date with time if no second argument was given.";
    }

    @Nonnull
    @Override
    public String getName() {
        return "isBefore";
    }
}
