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

/**
 * Performs a lookup for the helper of the given class.
 * <p>
 * This is actually a shortcut for {@code UserContext.getHelper(aClass)}. Note that in contrast to
 * {@link PartMacro} this isn't constant (as the helper is bound to the actual {@link sirius.web.security.ScopeInfo}).
 * However, this will be translated into {@link sirius.pasta.noodle.OpCode#OP_INTRINSIC_USER_CONTEXT_HELPER} and
 * therefore be quite efficient anyway.
 */
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
        throw new UnsupportedOperationException("unreachable");
    }

    @Override
    public void verify(CompilationContext context, Position position, List<Node> args) {
        if (args.size() != 1 || !args.get(0).isConstant() || !Class.class.isAssignableFrom(args.get(0).getType())) {
            throw new IllegalArgumentException("Expected a single constant class as argument.");
        }
    }

    @Override
    protected void verifyArguments(CompilationContext compilationContext, Position position, List<Class<?>> args) {
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
