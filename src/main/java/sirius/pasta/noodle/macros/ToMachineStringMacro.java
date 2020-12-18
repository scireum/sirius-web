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
import sirius.kernel.nls.NLS;
import sirius.pasta.noodle.Environment;
import sirius.pasta.noodle.compiler.CompilationContext;
import sirius.pasta.noodle.sandbox.PublicAPI;

import javax.annotation.Nonnull;
import java.util.List;

/**
 * Represents <tt>toMachineString(Object)</tt> which is a call to {@link NLS#toMachineString(Object)}.
 */
@Register
@PublicAPI
public class ToMachineStringMacro extends BasicMacro {
    @Override
    public Class<?> getType() {
        return String.class;
    }

    @Override
    public void verifyArguments(CompilationContext context, Position pos, List<Class<?>> args) {
        if (args.size() != 1) {
            throw new IllegalArgumentException("One parameter is expected");
        }
    }

    @Override
    public Object invoke(Environment environment, Object[] args) {
        return NLS.toMachineString(args[0]);
    }

    @Nonnull
    @Override
    public String getName() {
        return "toMachineString";
    }

    @Override
    public String getDescription() {
        return "Formats the given parameter as a machine processable string.";
    }
}
