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
import sirius.pasta.noodle.sandbox.PublicAPI;
import sirius.web.security.UserContext;

import javax.annotation.Nonnull;
import java.util.List;

@Register
@PublicAPI
public class HelperMacro extends BasicMacro {

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
    }

    @Override
    protected void verifyArguments(CompilationContext compilationContext, Position pos, List<Class<?>> args) {
        throw new UnsupportedOperationException("unreachable");
    }

    @Override
    public Object invoke(Environment environment, Object[] args) {
        return UserContext.getHelper((Class<?>) args[0]);
    }

    @Override
    public boolean isConstant(CompilationContext context, List<Node> args) {
        // The actual helper depends on the scope in which the template is rendered,
        // therefore this call is not constant.
        return false;
    }

    @Override
    public String getDescription() {
        return "Fetches the helper with the given type.";
    }

    @Nonnull
    @Override
    public String getName() {
        return "helper";
    }
}
