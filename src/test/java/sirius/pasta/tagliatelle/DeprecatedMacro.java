/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.tagliatelle;

import parsii.tokenizer.Position;
import sirius.kernel.di.std.Register;
import sirius.pasta.noodle.Environment;
import sirius.pasta.noodle.compiler.CompilationContext;
import sirius.pasta.noodle.compiler.ir.Node;
import sirius.pasta.noodle.macros.BasicMacro;

import javax.annotation.Nonnull;
import java.util.List;

/**
 * Helper-macro to check if calls to deprecated macros are detected.
 */
@Register
@Deprecated
public class DeprecatedMacro extends BasicMacro {

    @Override
    public Class<?> getType() {
        return String.class;
    }

    @Override
    public void verifyArguments(CompilationContext context, Position pos, List<Class<?>> args) {

    }

    @Override
    public Object invoke(Environment environment, Object[] args) {
        return "deprecated";
    }

    @Override
    public boolean isConstant(CompilationContext context, List<Node> args) {
        return false;
    }

    @Override
    public String getDescription() {
        return null;
    }

    @Nonnull
    @Override
    public String getName() {
        return "deprecatedMacro";
    }
}
