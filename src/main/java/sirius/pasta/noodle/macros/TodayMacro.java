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

import javax.annotation.Nonnull;
import java.time.LocalDate;
import java.util.List;

/**
 * Provides a shortcut for calling {@link LocalDate#now()}.
 */
@Register
@PublicAPI
public class TodayMacro extends BasicMacro {
    @Override
    public Class<?> getType() {
        return LocalDate.class;
    }

    @Override
    public void verifyArguments(CompilationContext context, Position position, List<Class<?>> args) {
        if (!args.isEmpty()) {
            throw new IllegalArgumentException("No parameters are expected!");
        }
    }

    @Override
    public Object invoke(Environment environment, Object[] args) {
        return LocalDate.now();
    }

    @Override
    public boolean isConstant(CompilationContext context, List<Node> args) {
        return false;
    }

    @Override
    public String getDescription() {
        return "Returns 'LocalDate.now()'";
    }

    @Nonnull
    @Override
    public String getName() {
        return "today";
    }
}
