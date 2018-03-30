/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.macros;

import sirius.kernel.di.std.Register;

import javax.annotation.Nonnull;
import java.time.LocalDateTime;

/**
 * Macro for comparing it a given date is after another given date or now.
 */
@Register(classes = Macro.class)
public class IsAfterMacro extends DateComparingMacro {

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
}
