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
 * Limit the given object to a given lenth.
 *
 * @see Strings#limit(Object, int)
 * @see Strings#limit(Object, int, boolean)
 */
@Register
@PublicAPI
public class LimitMacro extends BasicMacro {

    @Override
    public Class<?> getType() {
        return String.class;
    }

    @Override
    public void verifyArguments(CompilationContext context, Position pos, List<Class<?>> args) {
        if (args.size() < 2 || args.size() > 3 || !CompilationContext.isAssignableTo(args.get(1), int.class)) {
            throw new IllegalArgumentException(
                    "Expected the first argument to be an object and the second argument to be an integer.");
        }
        if (args.size() == 3 && !CompilationContext.isAssignableTo(args.get(2), boolean.class)) {
            throw new IllegalArgumentException("Expected the third argument to be a boolean.");
        }
    }

    @Override
    public Object invoke(Environment environment, Object[] args) {
        if (args.length == 3) {
            return Strings.limit(args[0], (int) args[1], (boolean) args[2]);
        } else {
            return Strings.limit(args[0], (int) args[1]);
        }
    }

    @Override
    public String getDescription() {
        return "Limits the given object.";
    }

    @Nonnull
    @Override
    public String getName() {
        return "limit";
    }
}
