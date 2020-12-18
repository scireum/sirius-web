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
import sirius.kernel.di.Injector;
import sirius.kernel.di.std.Register;
import sirius.pasta.noodle.Environment;
import sirius.pasta.noodle.compiler.CompilationContext;
import sirius.pasta.noodle.compiler.ir.Node;
import sirius.pasta.noodle.sandbox.PublicAPI;

import javax.annotation.Nonnull;
import java.util.List;

@Register
@PublicAPI
public class PartMacro extends BasicMacro {

    @Override
    public Class<?> getType(List<Node> args) {
        if (!args.isEmpty() && args.get(0).isConstant() && Class.class.isAssignableFrom(args.get(0).getType())) {
            return (Class<?>) args.get(0).getConstantValue();
        }

        return super.getType(args);
    }

    @Override
    protected Class<?> getType() {
        return void.class;
    }

    @Override
    public void verify(CompilationContext context, Position pos, List<Node> args) {
        if (args.size() != 1 || !args.get(0).isConstant() || !Class.class.isAssignableFrom(args.get(0).getType())) {
            throw new IllegalArgumentException("Expected a single constant class as argument.");
        }

        if (Injector.context().getPart((Class<?>) args.get(0).getConstantValue()) == null) {
            throw new IllegalArgumentException(Strings.apply(
                    "Unknown part requested: '%s'. Use 'Injector.context().getPart(Class) to fetch an optional part.",
                    args.get(0).getConstantValue()));
        }
    }

    @Override
    protected void verifyArguments(CompilationContext compilationContext, Position pos, List<Class<?>> args) {
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
}
