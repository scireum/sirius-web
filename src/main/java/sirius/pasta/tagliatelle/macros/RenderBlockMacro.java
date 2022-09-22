/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.tagliatelle.macros;

import sirius.kernel.tokenizer.Position;
import sirius.kernel.di.std.Register;
import sirius.pasta.noodle.Environment;
import sirius.pasta.noodle.compiler.CompilationContext;
import sirius.pasta.noodle.compiler.ir.Node;
import sirius.pasta.noodle.macros.BasicMacro;
import sirius.pasta.noodle.sandbox.PublicApi;
import sirius.pasta.tagliatelle.rendering.LocalRenderContext;

import javax.annotation.Nonnull;
import java.util.List;

/**
 * Returns the contents of the given block as string.
 */
@Register
@PublicApi
public class RenderBlockMacro extends BasicMacro {

    @Override
    public Class<?> getType() {
        return String.class;
    }

    @Override
    public void verifyArguments(CompilationContext context, Position pos, List<Class<?>> args) {
        if (args.size() != 1 || !CompilationContext.isAssignableTo(args.get(0), String.class)) {
            throw new IllegalArgumentException("Expected a single String as argument.");
        }
    }

    @Override
    public Object invoke(Environment environment, Object[] args) {
        LocalRenderContext renderContext = (LocalRenderContext) environment;
        return renderContext.getGlobalContext().emitToString(() -> {
            renderContext.emitBlock((String) args[0]);
        });
    }

    @Override
    public boolean isConstant(CompilationContext context, List<Node> args) {
        return false;
    }

    @Nonnull
    @Override
    public String getName() {
        return "renderToString";
    }

    @Override
    public String getDescription() {
        return "Renders the block with the given name into a string.";
    }
}
