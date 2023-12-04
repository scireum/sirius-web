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
import sirius.pasta.noodle.Environment;
import sirius.pasta.noodle.compiler.CompilationContext;
import sirius.pasta.noodle.compiler.ir.Node;
import sirius.pasta.noodle.sandbox.NoodleSandbox;

import javax.annotation.Nonnull;
import java.util.Arrays;
import java.util.List;

/**
 * Returns all values of a given enum class.
 */
@Register
@NoodleSandbox(NoodleSandbox.Accessibility.GRANTED)
public class EnumValuesMacro extends BasicMacro {

    @Override
    public Class<?> getType() {
        return List.class;
    }

    @Override
    public void verify(CompilationContext context, Position position, List<Node> args) {
        if (args.size() != 1
            || !CompilationContext.isAssignableTo(args.getFirst().getType(), Class.class)
            || !args.getFirst()
                    .isConstant()
            || !((Class<?>) args.getFirst().getConstantValue()).isEnum()) {
            throw new IllegalArgumentException("Expected an enum class as parameter.");
        }
    }

    @Override
    public void verifyArguments(CompilationContext context, Position position, List<Class<?>> args) {
        // unused...
    }

    @Override
    public Object invoke(Environment environment, Object[] args) {
        Class<?> type = (Class<?>) args[0];
        return Arrays.asList(type.getEnumConstants());
    }

    @Nonnull
    @Override
    public String getName() {
        return "enumValues";
    }

    @Override
    public String getDescription() {
        return "Returns a list of all enum constants of the given enum class.";
    }

    @Override
    public boolean isConstant(CompilationContext context, List<Node> args) {
        return true;
    }

}
