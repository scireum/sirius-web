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
import java.time.LocalDateTime;
import java.util.List;

/**
 * Provides a shortcut for calling {@link LocalDateTime#now()}.
 */
@Register
@NoodleSandbox(NoodleSandbox.Accessibility.GRANTED)
public class NowMacro extends BasicMacro {
    @Override
    public Class<?> getType() {
        return LocalDateTime.class;
    }

    @Override
    public void verifyArguments(CompilationContext context, Position position, List<Class<?>> args) {
        if (!args.isEmpty()) {
            throw new IllegalArgumentException("No parameters are expected!");
        }
    }

    @Override
    public Object invoke(Environment environment, Object[] args) {
        return LocalDateTime.now();
    }

    @Override
    public boolean isConstant(CompilationContext context, List<Node> args) {
        return false;
    }

    @Override
    public String getDescription() {
        return "Returns 'LocalDateTime.now()'";
    }

    @Nonnull
    @Override
    public String getName() {
        return "now";
    }
}
