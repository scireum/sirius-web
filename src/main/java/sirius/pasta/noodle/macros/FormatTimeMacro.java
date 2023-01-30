/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.macros;

import sirius.kernel.commons.Value;
import sirius.kernel.di.std.Register;
import sirius.kernel.nls.NLS;
import sirius.pasta.noodle.Environment;
import sirius.pasta.noodle.sandbox.NoodleSandbox;

import javax.annotation.Nonnull;
import java.time.LocalTime;
import java.time.temporal.TemporalAccessor;
import java.util.Date;

/**
 * Represents <tt>formatTime(…)</tt>.
 * <p>
 * The macro supports the datatypes {@code long} for epoch milliseconds, {@link Date} for legacy applications, and
 * {@link TemporalAccessor}. The invocation is forwarded to {@link NLS#toUserString(Object)} via
 * {@link Value#asLocalTime(LocalTime)}.
 */
@Register
@NoodleSandbox(NoodleSandbox.Accessibility.GRANTED)
public class FormatTimeMacro extends FormatDateMacro {

    @Override
    public Object invoke(Environment environment, Object[] args) {
        Value evalResult = Value.of(args[0]);
        if (evalResult.isNull()) {
            return "";
        }
        return NLS.toUserString(evalResult.asLocalTime(null));
    }

    @Nonnull
    @Override
    public String getName() {
        return "formatTime";
    }

    @Override
    public String getDescription() {
        return "Formats the given parameter as time.";
    }
}
