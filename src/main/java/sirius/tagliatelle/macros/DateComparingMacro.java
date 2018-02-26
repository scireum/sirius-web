/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.macros;

import sirius.kernel.nls.NLS;
import sirius.tagliatelle.Tagliatelle;
import sirius.tagliatelle.expression.Expression;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;

/**
 * Basic class for {@link Macro macros} handling date comparison
 */
public abstract class DateComparingMacro implements Macro {

    private static final String WRONG_ARGUMENT_EXCEPTION =
            "Expected one or two arguments of type String, LocalDate or LocalDateTime.";

    @Override
    public Class<?> getType() {
        return Boolean.class;
    }

    protected LocalDateTime parseInput(Object object) {
        if (object instanceof String) {
            return NLS.parseUserString(LocalDateTime.class, (String) object);
        }
        if (object instanceof LocalDate) {
            return ((LocalDate) object).atStartOfDay();
        }
        if (object instanceof LocalDateTime) {
            return (LocalDateTime) object;
        }
        throw new IllegalArgumentException("Input cannot be parsed");
    }

    @Override
    public void verifyArguments(List<Expression> args) {
        if (args.size() > 2 || args.isEmpty()) {
            throw new IllegalArgumentException(WRONG_ARGUMENT_EXCEPTION);
        }

        if (!Tagliatelle.isAssignableTo(args.get(0).getType(), String.class)
            && !Tagliatelle.isAssignableTo(args.get(0)
                                               .getType(),
                                           LocalDate.class)
            && !Tagliatelle.isAssignableTo(args.get(0).getType(), LocalDateTime.class)) {
            throw new IllegalArgumentException(WRONG_ARGUMENT_EXCEPTION);
        }

        if (args.size() == 1) {
            return;
        }
        if (!Tagliatelle.isAssignableTo(args.get(1).getType(), String.class)
            && !Tagliatelle.isAssignableTo(args.get(1)
                                               .getType(),
                                           LocalDate.class)
            && !Tagliatelle.isAssignableTo(args.get(1).getType(), LocalDateTime.class)) {
            throw new IllegalArgumentException(WRONG_ARGUMENT_EXCEPTION);
        }
    }
}
