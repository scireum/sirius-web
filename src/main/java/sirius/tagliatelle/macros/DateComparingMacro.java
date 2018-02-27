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
import sirius.tagliatelle.rendering.LocalRenderContext;

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

    @Override
    public boolean isConstant(Expression[] args) {
        return true;
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
    public Object eval(LocalRenderContext ctx, Expression[] args) {
        LocalDateTime firstDate = parseInput(args[0].eval(ctx));
        LocalDateTime secondDate = (args.length == 2 ? parseInput(args[1].eval(ctx)) : LocalDateTime.now());

        return compare(firstDate, secondDate);
    }

    protected abstract boolean compare(LocalDateTime firstDate, LocalDateTime secondDate);

    @Override
    public void verifyArguments(List<Expression> args) {
        if (args.size() > 2 || args.isEmpty()) {
            throw new IllegalArgumentException(WRONG_ARGUMENT_EXCEPTION);
        }

        checkClassAssignable(args.get(0).getType());

        if (args.size() == 1) {
            return;
        }

        checkClassAssignable(args.get(1).getType());
    }

    private void checkClassAssignable(Class<?> type) {
        if (!Tagliatelle.isAssignableTo(type, String.class)
            && !Tagliatelle.isAssignableTo(type, LocalDate.class)
            && !Tagliatelle.isAssignableTo(type, LocalDateTime.class)) {
            throw new IllegalArgumentException(WRONG_ARGUMENT_EXCEPTION);
        }
    }
}
