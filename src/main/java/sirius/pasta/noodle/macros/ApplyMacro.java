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
import sirius.pasta.noodle.sandbox.PublicAPI;

import javax.annotation.Nonnull;
import java.util.List;

/**
 * Formats the given pattern string with the given arguments.
 *
 * @see Strings#apply(String, Object...)
 */
@Register
@PublicAPI
public class ApplyMacro extends BasicMacro {
    @Override
    public Class<?> getType() {
        return String.class;
    }

    @Override
    public void verifyArguments(CompilationContext context, Position position, List<Class<?>> args) {
        if (args.isEmpty() || !CompilationContext.isAssignableTo(args.get(0), String.class)) {
            throw new IllegalArgumentException("Expected at least a String as first argument.");
        }
    }

    @Override
    public Object invoke(Environment environment, Object[] args) {
        Object[] parameters = new Object[args.length - 1];
        System.arraycopy(args, 1, parameters, 0, args.length - 1);

        return Strings.apply((String) args[0], parameters);
    }

    @Nonnull
    @Override
    public String getName() {
        return "apply";
    }

    @Override
    public String getDescription() {
        return "Generates a string based on the pattern and the additionally given parameters.";
    }
}
