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
import sirius.kernel.nls.Formatter;
import sirius.pasta.noodle.Environment;
import sirius.pasta.noodle.compiler.CompilationContext;
import sirius.pasta.noodle.compiler.ir.Node;
import sirius.pasta.noodle.sandbox.PublicAPI;

import javax.annotation.Nonnull;
import java.util.List;

/**
 * Permits to create a new {@link Formatter} via a <tt>{@literal @}format(pattern)</tt>.
 */
@Register
@PublicAPI
public class FormatMacro extends BasicMacro {

    @Override
    public Class<?> getType() {
        return Formatter.class;
    }

    @Override
    public void verifyArguments(CompilationContext context, Position position, List<Class<?>> args) {
        if (args.size() != 1 || !CompilationContext.isAssignableTo(args.get(0), String.class)) {
            throw new IllegalArgumentException("Expected a single String as argument.");
        }
    }

    @Override
    public Object invoke(Environment environment, Object[] args) {
        return Formatter.create((String) args[0]);
    }

    @Nonnull
    @Override
    public String getName() {
        return "format";
    }

    @Override
    public String getDescription() {
        return "Creates a new Formatter object for the given pattern.";
    }

    @Override
    public boolean isConstant(CompilationContext context, List<Node> args) {
        return true;
    }

}
