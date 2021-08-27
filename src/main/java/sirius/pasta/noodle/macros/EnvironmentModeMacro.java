/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.macros;

import parsii.tokenizer.Position;
import sirius.pasta.noodle.compiler.CompilationContext;
import sirius.pasta.noodle.compiler.ir.Node;

import java.util.List;

/**
 * Base class for environment determining {@link Macro macros}.
 */
public abstract class EnvironmentModeMacro extends BasicMacro {

    @Override
    public Class<?> getType() {
        return boolean.class;
    }

    @Override
    protected void verifyArguments(CompilationContext compilationContext, Position position, List<Class<?>> args) {
        if (!args.isEmpty()) {
            throw new IllegalArgumentException("No arguments expected!");
        }
    }

    @Override
    public boolean isConstant(CompilationContext context, List<Node> args) {
        return true;
    }

}
