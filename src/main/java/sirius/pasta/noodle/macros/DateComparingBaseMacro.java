/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.macros;

import parsii.tokenizer.Position;
import sirius.kernel.nls.NLS;
import sirius.pasta.noodle.Environment;
import sirius.pasta.noodle.compiler.CompilationContext;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;

/**
 * Basic class for {@link Macro macros} handling date comparison.
 */
public abstract class DateComparingBaseMacro extends BasicMacro {

    @Override
    public Class<?> getType() {
        return Boolean.class;
    }

    @Override
    public void verifyArguments(CompilationContext context, Position position, List<Class<?>> args) {
        if (args.size() > 2 || args.isEmpty()) {
            throwWrongArgumentsException();
            return;
        }

        checkClassAssignable(args.get(0));

        if (args.size() > 1) {
            checkClassAssignable(args.get(1));
        }
    }

    protected void throwWrongArgumentsException() {
        throw new IllegalArgumentException("Expected one or two arguments of type String, LocalDate or LocalDateTime.");
    }

    private void checkClassAssignable(Class<?> type) {
        if (!CompilationContext.isAssignableTo(type, String.class)
            && !CompilationContext.isAssignableTo(type,
                                                  LocalDate.class)
            && !CompilationContext.isAssignableTo(type, LocalDateTime.class)) {
            throwWrongArgumentsException();
        }
    }

    @Override
    public Object invoke(Environment environment, Object[] args) {
        LocalDateTime firstDate = parseInput(args[0]);
        LocalDateTime secondDate = args.length == 2 ? parseInput(args[1]) : LocalDateTime.now();

        return compare(firstDate, secondDate);
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

    protected abstract boolean compare(LocalDateTime firstDate, LocalDateTime secondDate);
}
