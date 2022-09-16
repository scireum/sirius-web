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
import sirius.pasta.noodle.Environment;
import sirius.pasta.noodle.compiler.CompilationContext;
import sirius.pasta.noodle.compiler.ir.Node;
import sirius.pasta.noodle.sandbox.PublicApi;

import javax.annotation.Nonnull;
import java.util.Arrays;
import java.util.List;

/**
 * Represents a list containing all given parameters. If only one parameter of an array type is given, it will be
 * expanded into the list instead.
 */
@Register
@PublicApi
public class ToListMacro extends BasicMacro {

    @Override
    public Class<?> getType() {
        return List.class;
    }

    @Override
    public void verifyArguments(CompilationContext context, Position position, List<Class<?>> args) {
        if (args.isEmpty()) {
            throw new IllegalArgumentException("Expected at least one parameter");
        }
    }

    @Override
    public Object invoke(Environment environment, Object[] args) {
        if (args.length == 1 && args[0].getClass().isArray()) {
            return Arrays.stream((Object[]) args[0]).toList();
        } else {
            return Arrays.stream(args).toList();
        }
    }

    @Nonnull
    @Override
    public String getName() {
        return "toList";
    }

    @Override
    public String getDescription() {
        return "Returns a list containing of the given items, or the given array";
    }

    @Override
    public boolean isConstant(CompilationContext context, List<Node> args) {
        return true;
    }
}
