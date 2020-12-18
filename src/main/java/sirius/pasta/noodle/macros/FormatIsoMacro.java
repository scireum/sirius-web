/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.macros;

import parsii.tokenizer.Position;
import sirius.kernel.di.std.Register;
import sirius.pasta.noodle.Environment;
import sirius.pasta.noodle.compiler.CompilationContext;
import sirius.pasta.noodle.sandbox.PublicAPI;

import javax.annotation.Nonnull;
import java.time.LocalDate;
import java.time.LocalTime;
import java.time.Year;
import java.time.YearMonth;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.time.format.SignStyle;
import java.time.temporal.ChronoField;
import java.time.temporal.Temporal;
import java.util.List;

/**
 * Formats the given parameter as a value string to be inserted into an HTML input element.
 * <table>
 * <tr><td>{@link LocalDate}</td><td>{@code <input type="date">}</td></tr>
 * <tr><td>{@link LocalTime}</td><td>{@code <input type="time">}</td></tr>
 * <tr><td>{@link YearMonth}</td><td>{@code <input type="month">}</td></tr>
 * <tr><td>{@link Year}</td><td>{@code <input type="number">}</td></tr>
 * <tr><td>any other subclass of {@link Temporal}</td><td>{@code <input type="datetime-local">}</td></tr>
 * </table>
 */
@Register
@PublicAPI
public class FormatIsoMacro extends BasicMacro {

    private static final DateTimeFormatter ISO_LOCAL_YEAR =
            new DateTimeFormatterBuilder().appendValue(ChronoField.YEAR, 4, 10, SignStyle.EXCEEDS_PAD).toFormatter();

    private static final DateTimeFormatter ISO_LOCAL_YEAR_MONTH =
            new DateTimeFormatterBuilder().appendValue(ChronoField.YEAR, 4, 10, SignStyle.EXCEEDS_PAD)
                                          .appendLiteral('-')
                                          .appendValue(ChronoField.MONTH_OF_YEAR, 2)
                                          .toFormatter();

    @Override
    public Class<?> getType() {
        return String.class;
    }

    @Override
    public void verifyArguments(CompilationContext context, Position position, List<Class<?>> args) {
        if (args.size() != 1 || !CompilationContext.isAssignableTo(args.get(0), Temporal.class)) {
            throw new IllegalArgumentException("One parameter of type Temporal is expected.");
        }
    }

    @Override
    public Object invoke(Environment environment, Object[] args) {
        Object temporal = args[0];
        if (temporal == null) {
            return "";
        }

        if (temporal instanceof Year) {
            return ISO_LOCAL_YEAR.format((Year) temporal);
        }
        if (temporal instanceof YearMonth) {
            return ISO_LOCAL_YEAR_MONTH.format((YearMonth) temporal);
        }
        if (temporal instanceof LocalDate) {
            return DateTimeFormatter.ISO_LOCAL_DATE.format((LocalDate) temporal);
        }
        if (temporal instanceof LocalTime) {
            return DateTimeFormatter.ISO_LOCAL_TIME.format((LocalTime) temporal);
        }

        return DateTimeFormatter.ISO_LOCAL_DATE_TIME.format((Temporal) temporal);
    }

    @Nonnull
    @Override
    public String getName() {
        return "formatISO";
    }

    @Override
    public String getDescription() {
        return "Formats the given parameter as a value string to be inserted into an HTML input element.";
    }
}
