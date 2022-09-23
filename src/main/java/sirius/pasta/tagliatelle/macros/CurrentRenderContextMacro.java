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
import sirius.pasta.tagliatelle.rendering.LocalRenderContext;

import javax.annotation.Nonnull;
import java.util.List;

/**
 * Makes the current {@link LocalRenderContext} available.
 * <p>
 * Note that this context should be handled with care, as it can impact the rendering process pretty deeply. One
 * possible use-case is to call {@link LocalRenderContext#getRootContext()} in order to obtain the name / path of the
 * template being currently rendered.
 */
@Register
public class CurrentRenderContextMacro extends BasicMacro {

    @Override
    protected Class<?> getType() {
        return LocalRenderContext.class;
    }

    @Override
    protected void verifyArguments(CompilationContext compilationContext, Position position, List<Class<?>> args) {
        if (!args.isEmpty()) {
            throw new IllegalArgumentException("This macro expects no arguments");
        }
    }

    @Override
    public boolean isConstant(CompilationContext context, List<Node> args) {
        return false;
    }

    @Override
    public Object invoke(Environment environment, Object[] args) {
        return environment;
    }

    @Override
    public String getDescription() {
        return "Provides the currently active LocalRenderContext.";
    }

    @Nonnull
    @Override
    public String getName() {
        return "currentRenderContext";
    }
}
