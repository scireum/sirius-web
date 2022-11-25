/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.macros;

import sirius.kernel.tokenizer.Position;
import sirius.kernel.di.std.Register;
import sirius.kernel.nls.NLS;
import sirius.pasta.noodle.Environment;
import sirius.pasta.noodle.compiler.CompilationContext;
import sirius.pasta.noodle.compiler.ir.Node;
import sirius.pasta.noodle.sandbox.PublicApi;

import javax.annotation.Nonnull;
import java.time.temporal.Temporal;
import java.util.List;

/**
 * Represents <tt>toSpokenDate(Temporal)</tt> which is a call to {@link NLS#toSpokenDate(Temporal)}.
 */
@Register
@PublicApi
public class ToSpokenDateMacro extends BasicMacro {
    @Override
    public Class<?> getType() {
        return String.class;
    }

    @Override
    public void verifyArguments(CompilationContext context, Position position, List<Class<?>> args) {
        if (args.size() != 1 || !CompilationContext.isAssignableTo(args.get(0), Temporal.class)) {
            throw new IllegalArgumentException("Expected a single Temporal argument");
        }
    }

    @Override
    public Object invoke(Environment environment, Object[] args) {
        return NLS.toSpokenDate((Temporal) args[0]);
    }

    @Override
    public boolean isConstant(CompilationContext context, List<Node> args) {
        return false;
    }

    @Nonnull
    @Override
    public String getName() {
        return "toSpokenDate";
    }

    @Override
    public String getDescription() {
        return "Converts dates to a \"human\" (e.g. \"today\", \"yesterday\") format.";
    }
}
