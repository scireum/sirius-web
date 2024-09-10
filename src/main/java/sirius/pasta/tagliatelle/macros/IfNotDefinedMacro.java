/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.tagliatelle.macros;

import sirius.kernel.commons.Strings;
import sirius.kernel.di.std.Register;
import sirius.kernel.tokenizer.Position;
import sirius.pasta.noodle.Environment;
import sirius.pasta.noodle.compiler.CompilationContext;
import sirius.pasta.noodle.compiler.ir.Node;
import sirius.pasta.noodle.macros.BasicMacro;
import sirius.pasta.tagliatelle.rendering.GlobalRenderContext;
import sirius.pasta.tagliatelle.rendering.LocalRenderContext;

import javax.annotation.Nonnull;
import java.util.List;

/**
 * Checks whether the given {@linkplain GlobalRenderContext#hasGuard(String) guard } is defined or not.
 *
 * @see GlobalRenderContext#addGuard(String)
 */
@Register
public class IfNotDefinedMacro extends BasicMacro {

    @Override
    protected Class<?> getType() {
        return boolean.class;
    }

    @Override
    protected void verifyArguments(CompilationContext compilationContext, Position position, List<Class<?>> args) {
        if (args.size() != 1) {
            throw new IllegalArgumentException("Expected a single String as argument.");
        }
    }

    @Override
    public boolean isConstant(CompilationContext context, List<Node> args) {
        return false;
    }

    @Override
    public Object invoke(Environment environment, Object[] args) {
        String guard = (String) args[0];
        if (Strings.isFilled(guard) && environment instanceof LocalRenderContext localRenderContext) {
            GlobalRenderContext globalRenderContext = localRenderContext.getGlobalContext();
            if (globalRenderContext.hasGuard(guard)) {
                return false;
            } else {
                globalRenderContext.addGuard(guard);
                return true;
            }
        }

        return false;
    }

    @Override
    public String getDescription() {
        return "Checks whether the given guard is defined or not. If the guard is not defined, true is returned and the guard is added to a Set of guards to the global context of the template. This means that all subsequent calls to ifNotDefined with the same guard will return false. This allows to run a block/code exactly once per template. The behavior is somewhat similar to C/C++ #ifndef.";
    }

    @Nonnull
    @Override
    public String getName() {
        return "ifNotDefined";
    }
}
