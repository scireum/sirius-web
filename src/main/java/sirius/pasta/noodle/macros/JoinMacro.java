/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.macros;

import parsii.tokenizer.Position;
import sirius.kernel.commons.Strings;
import sirius.kernel.di.std.Register;
import sirius.pasta.noodle.Environment;
import sirius.pasta.noodle.compiler.CompilationContext;
import sirius.pasta.noodle.compiler.ir.Node;
import sirius.pasta.noodle.sandbox.PublicApi;

import javax.annotation.Nonnull;
import java.util.List;

/**
 * Represents <tt>join(Iterable, String)</tt> which is a call to {@link Strings#join(Iterable, String)}.
 */
@Register
@PublicApi
public class JoinMacro extends BasicMacro {
    @Override
    public Class<?> getType() {
        return String.class;
    }

    @Override
    public void verifyArguments(CompilationContext context, Position position, List<Class<?>> args) {
        if (args.size() != 2
            || !CompilationContext.isAssignableTo(args.get(0), Iterable.class)
            || !CompilationContext.isAssignableTo(args.get(1), String.class)) {
            throw new IllegalArgumentException(
                    "Expected the first parameter to be an Iterable and the second to be a String");
        }
    }

    @Override
    public Object invoke(Environment environment, Object[] args) {
        return Strings.join((Iterable<?>) args[0], (String) args[1]);
    }

    @Nonnull
    @Override
    public String getName() {
        return "join";
    }

    @Override
    public String getDescription() {
        return "Returns a string concatenation of the given lists items";
    }

    @Override
    public boolean isConstant(CompilationContext context, List<Node> args) {
        return true;
    }

}
