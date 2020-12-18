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
 * Represents <tt>isFilled(Object)</tt> which is a call to {@link Strings#isFilled(Object)}.
 */
@Register
@PublicAPI
public class IsFilledMacro extends BasicMacro {

    @Override
    public Class<?> getType() {
        return boolean.class;
    }

    @Override
    public void verifyArguments(CompilationContext context, Position pos, List<Class<?>> args) {
        if (args.size() != 1) {
            throw new IllegalArgumentException("Expected a single String as argument.");
        }
    }

    @Override
    public Object invoke(Environment environment, Object[] args) {
        return Strings.isFilled(args[0]);
    }

    @Nonnull
    @Override
    public String getName() {
        return "isFilled";
    }

    @Override
    public String getDescription() {
        return "Returns true, if a non empty string is given as parameter, false otherwise.";
    }
}
