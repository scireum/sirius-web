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
import sirius.web.security.UserInfo;

import javax.annotation.Nonnull;
import java.util.List;

@Register
@PublicAPI
public class UserMacro extends BasicMacro {

    @Override
    protected Class<?> getType() {
        return UserInfo.class;
    }

    @Override
    protected void verifyArguments(CompilationContext compilationContext, Position pos, List<Class<?>> args) {
        if (!args.isEmpty()) {
            throw new IllegalArgumentException("No arguments are expected.");
        }
    }

    @Override
    public Object invoke(Environment environment, Object[] args) {
        return UserContext.getCurrentUser();
    }

    @Override
    public boolean isConstant(CompilationContext context, List<Node> args) {
        return false;
    }

    @Override
    public String getDescription() {
        return "Returns the current user as reported by UserContexr.getCurrentUser()";
    }

    @Nonnull
    @Override
    public String getName() {
        return "user";
    }
}
