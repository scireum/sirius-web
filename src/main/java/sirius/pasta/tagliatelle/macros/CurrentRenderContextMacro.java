/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.tagliatelle.macros;

import parsii.tokenizer.Position;
import sirius.kernel.di.std.Register;
import sirius.pasta.noodle.Environment;
import sirius.pasta.noodle.compiler.CompilationContext;
import sirius.pasta.noodle.compiler.ir.Node;
import sirius.pasta.noodle.macros.BasicMacro;
import sirius.pasta.tagliatelle.rendering.LocalRenderContext;

import javax.annotation.Nonnull;
import java.util.List;

@Register
public class CurrentRenderContextMacro extends BasicMacro {

    @Override
    protected Class<?> getType() {
        return LocalRenderContext.class;
    }

    @Override
    protected void verifyArguments(CompilationContext compilationContext, Position position, List<Class<?>> args) {

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
        return null;
    }

    @Nonnull
    @Override
    public String getName() {
        return "currentRenderContext";
    }
}
