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
 * Pads the given string with the given arguments.
 *
 * @see Strings#rightPad(String, String, int)
 */
@Register
@PublicAPI
public class RightPadMacro extends BasicMacro {

    @Override
    public Class<?> getType() {
        return String.class;
    }

    @Override
    public void verifyArguments(CompilationContext context, Position position, List<Class<?>> args) {
        if (args.size() != 3
            || !CompilationContext.isAssignableTo(args.get(0), String.class)
            || !CompilationContext.isAssignableTo(args.get(1), String.class)
            || !CompilationContext.isAssignableTo(args.get(2), int.class)) {
            throw new IllegalArgumentException("Expected the first and second argument to be a string "
                                               + "and the third to be a integer.");
        }
    }

    @Override
    public Object invoke(Environment environment, Object[] args) {
        return Strings.rightPad((String) args[0], (String) args[1], (int) args[2]);
    }

    @Override
    public String getDescription() {
        return "Right pads the given string.";
    }

    @Nonnull
    @Override
    public String getName() {
        return "rightPad";
    }
}
