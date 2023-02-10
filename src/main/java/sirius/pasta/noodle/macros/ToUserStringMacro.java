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
import sirius.kernel.nls.NLS;
import sirius.pasta.noodle.Environment;
import sirius.pasta.noodle.compiler.CompilationContext;
import sirius.pasta.noodle.compiler.ir.Node;
import sirius.pasta.noodle.sandbox.NoodleSandbox;

import javax.annotation.Nonnull;
import java.util.List;

/**
 * Represents <tt>toUserString(Object)</tt> which is a call to {@link NLS#toUserString(Object)}.
 */
@Register
@NoodleSandbox(NoodleSandbox.Accessibility.GRANTED)
public class ToUserStringMacro extends BasicMacro {
    @Override
    public Class<?> getType() {
        return String.class;
    }

    @Override
    public void verifyArguments(CompilationContext context, Position position, List<Class<?>> args) {
        if (args.size() != 1) {
            throw new IllegalArgumentException("One parameter is expected");
        }
    }

    @Override
    public Object invoke(Environment environment, Object[] args) {
        return NLS.toUserString(args[0]);
    }

    @Override
    public boolean isConstant(CompilationContext context, List<Node> args) {
        return false;
    }

    @Nonnull
    @Override
    public String getName() {
        return "toUserString";
    }

    @Override
    public String getDescription() {
        return "Formats the given parameter as a user readable string.";
    }
}
