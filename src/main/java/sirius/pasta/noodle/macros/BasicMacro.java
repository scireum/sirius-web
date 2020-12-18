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
import java.util.stream.Collectors;

/**
 * Provides a base class for all macros which only check the types of the given parameters.
 */
public abstract class BasicMacro implements Macro {

    @Override
    public Class<?> getType(List<Node> args) {
        return getType();
    }

    protected abstract Class<?> getType();

    @Override
    public void verify(CompilationContext context, Position pos, List<Node> args) {
        verifyArguments(context, pos, args.stream().map(Node::getType).collect(Collectors.toList()));
    }

    /**
     * Verifies the arguments of the macro based on their types.
     *
     * @param compilationContext the context used to add additional errors or warnings
     * @param pos                the position within the source code
     * @param args               the types of the available parameters
     * @throws IllegalArgumentException in case of invalid arguments
     */
    protected abstract void verifyArguments(CompilationContext compilationContext, Position pos, List<Class<?>> args);

    @Override
    public boolean isConstant(CompilationContext context, List<Node> args) {
        return true;
    }
}
