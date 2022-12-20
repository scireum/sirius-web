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
import sirius.pasta.noodle.sandbox.PublicApi;

import javax.annotation.Nonnull;
import java.time.temporal.TemporalAccessor;
import java.util.Date;

/**
 * Represents <tt>formatDateTimeWithoutSeconds(…)</tt>.
 * <p>
 * The macro supports the datatypes {@code long} for epoch milliseconds, {@link Date} for legacy applications, and
 * {@link TemporalAccessor}. The invocation is forwarded to {@link NLS#getDateTimeFormatWithoutSeconds(String)#format}
 * via {@link Value#asLocalDateTime(java.time.LocalDateTime)} and using {@link NLS#getCurrentLanguage()}.
 */
@Register
@PublicApi
public class FormatDateTimeWithoutSecondsMacro extends FormatDateMacro {

    @Override
    public Object invoke(Environment environment, Object[] args) {
        Value evalResult = Value.of(args[0]);
        if (evalResult.isNull()) {
            return "";
        }
        return NLS.getDateTimeFormatWithoutSeconds(NLS.getCurrentLanguage()).format(evalResult.asLocalDateTime(null));
    }

    @Nonnull
    @Override
    public String getName() {
        return "formatDateTimeWithoutSeconds";
    }

    @Override
    public String getDescription() {
        return "Formats the given parameter as date with time but without seconds.";
    }
}
