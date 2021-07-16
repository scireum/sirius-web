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
import sirius.kernel.commons.Tuple;
import sirius.kernel.di.std.Register;
import sirius.pasta.noodle.Environment;
import sirius.pasta.noodle.compiler.CompilationContext;
import sirius.pasta.noodle.compiler.ir.Node;

import javax.annotation.Nonnull;
import java.util.List;

/**
 * Splits the given {@link String} by the first occurence of the given separator.
 *
 * @see Strings#split(String, String)
 * @deprecated Use <tt>Strings.split</tt> as it provides the proper generic types for the returned tuple.
 */
@Register
@Deprecated
public class SplitMacro extends BasicMacro {

    @Override
    public Class<?> getType() {
        return Tuple.class;
    }

    @Override
    public void verifyArguments(CompilationContext context, Position position, List<Class<?>> args) {
        if (args.size() != 2
            || !CompilationContext.isAssignableTo(args.get(0), String.class)
            || !CompilationContext.isAssignableTo(args.get(1), String.class)) {
            throw new IllegalArgumentException("Expected two strings as arguments.");
        }
    }

    @Override
    public Object invoke(Environment environment, Object[] args) {
        return Strings.split((String) args[0], (String) args[1]);
    }

    @Override
    public String getDescription() {
        return "Splits the given string on the first occurence of the given separator.";
    }

    @Nonnull
    @Override
    public String getName() {
        return "split";
    }


    @Override
    public boolean isConstant(CompilationContext context, List<Node> args) {
        return true;
    }

}
