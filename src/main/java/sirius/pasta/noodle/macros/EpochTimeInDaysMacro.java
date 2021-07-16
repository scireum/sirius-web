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
import java.util.List;
import java.util.concurrent.TimeUnit;

/**
 * Gets the current time since 1970 in days.
 */
@Register
@PublicAPI
public class EpochTimeInDaysMacro extends BasicMacro {

    @Override
    public Class<?> getType() {
        return String.class;
    }

    @Override
    public void verifyArguments(CompilationContext context, Position position, List<Class<?>> args) {
        if (!args.isEmpty()) {
            throw new IllegalArgumentException("'epochTimeInDays' does not take arguments.");
        }
    }

    @Override
    public Object invoke(Environment environment, Object[] args) {
        return TimeUnit.DAYS.convert(System.currentTimeMillis(), TimeUnit.MILLISECONDS);
    }

    @Nonnull
    @Override
    public String getName() {
        return "epochTimeInDays";
    }

    @Override
    public String getDescription() {
        return "Gets the current time since 1970 in days";
    }

    @Override
    public boolean isConstant(CompilationContext context, List<Node> args) {
        return true;
    }

}
