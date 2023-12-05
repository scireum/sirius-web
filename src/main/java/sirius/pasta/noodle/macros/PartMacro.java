/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.macros;

import sirius.kernel.commons.Strings;
import sirius.kernel.di.Injector;
import sirius.kernel.di.std.Register;
import sirius.kernel.tokenizer.Position;
import sirius.pasta.noodle.Environment;
import sirius.pasta.noodle.compiler.CompilationContext;
import sirius.pasta.noodle.compiler.ir.Node;
import sirius.pasta.noodle.sandbox.NoodleSandbox;

import javax.annotation.Nonnull;
import java.util.List;

/**
 * Performs a lookup for the given part.
 * <p>
 * This is a shortcut for {@code Injector.context().getPart(type)}. Note that due to the static nature of the system
 * this is a constant call and will be optimized away entirely.
 */
@Register
@NoodleSandbox(NoodleSandbox.Accessibility.GRANTED)
public class PartMacro extends BasicMacro {

    @Override
    public Class<?> getType(List<Node> args) {
        if (!args.isEmpty() && args.getFirst().isConstant() && Class.class.isAssignableFrom(args.getFirst().getType())) {
            return (Class<?>) args.getFirst().getConstantValue();
        }

        return super.getType(args);
    }

    @Override
    protected Class<?> getType() {
        return void.class;
    }

    @Override
    public void verify(CompilationContext context, Position position, List<Node> args) {
        if (args.size() != 1 || !args.getFirst().isConstant() || !Class.class.isAssignableFrom(args.getFirst().getType())) {
            throw new IllegalArgumentException("Expected a single constant class as argument.");
        }

        if (Injector.context().getPart((Class<?>) args.getFirst().getConstantValue()) == null) {
            throw new IllegalArgumentException(Strings.apply(
                    "Unknown part requested: '%s'. Use 'Injector.context().getPart(Class) to fetch an optional part.",
                    args.getFirst().getConstantValue()));
        }
    }

    @Override
    protected void verifyArguments(CompilationContext compilationContext, Position position, List<Class<?>> args) {
        throw new UnsupportedOperationException("unreachable");
    }

    @Override
    public Object invoke(Environment environment, Object[] args) {
        return Injector.context().getPart((Class<?>) args[0]);
    }

    @Override
    public String getDescription() {
        return "Uses the Injector to fetch the part with the given type.";
    }

    @Nonnull
    @Override
    public String getName() {
        return "part";
    }

    @Override
    public boolean isConstant(CompilationContext context, List<Node> args) {
        return true;
    }
}
