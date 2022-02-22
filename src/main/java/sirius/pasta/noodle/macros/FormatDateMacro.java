/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.macros;

import parsii.tokenizer.Position;
import sirius.kernel.commons.Value;
import sirius.kernel.di.std.Register;
import sirius.kernel.nls.NLS;
import sirius.pasta.noodle.Environment;
import sirius.pasta.noodle.compiler.CompilationContext;
import sirius.pasta.noodle.compiler.ir.Node;
import sirius.pasta.noodle.sandbox.PublicApi;

import javax.annotation.Nonnull;
import java.time.LocalDate;
import java.time.temporal.TemporalAccessor;
import java.util.Date;
import java.util.List;

/**
 * Represents <tt>formatDate(…)</tt>.
 * <p>
 * The macro supports the datatypes {@code long} for epoch milliseconds, {@link Date} for legacy applications, and
 * {@link TemporalAccessor}. The invocation is forwarded to {@link NLS#toUserString(Object)} via
 * {@link Value#asLocalDate(LocalDate)}.
 */
@Register
@PublicApi
public class FormatDateMacro extends BasicMacro {

    @Override
    public Class<?> getType() {
        return String.class;
    }

    @Override
    public void verifyArguments(CompilationContext context, Position position, List<Class<?>> args) {
        if (args.size() != 1) {
            throw new IllegalArgumentException("One parameter is expected");
        }

        if (!CompilationContext.isAssignableTo(args.get(0), Long.class)
            && !CompilationContext.isAssignableTo(args.get(0), Date.class)
            && !CompilationContext.isAssignableTo(args.get(0), TemporalAccessor.class)) {
            throw new IllegalArgumentException("Illegal parameter type");
        }
    }

    @Override
    public Object invoke(Environment environment, Object[] args) {
        Value evalResult = Value.of(args[0]);
        if (evalResult.isNull()) {
            return "";
        }
        return NLS.toUserString(evalResult.asLocalDate(null));
    }

    @Override
    public boolean isConstant(CompilationContext context, List<Node> args) {
        return false;
    }

    @Nonnull
    @Override
    public String getName() {
        return "formatDate";
    }

    @Override
    public String getDescription() {
        return "Formats the given parameter as date.";
    }
}
