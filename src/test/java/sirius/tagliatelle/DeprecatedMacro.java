/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle;

import parsii.tokenizer.Position;
import sirius.kernel.di.std.Register;
import sirius.tagliatelle.compiler.CompilationContext;
import sirius.tagliatelle.expression.Expression;
import sirius.tagliatelle.macros.Macro;
import sirius.tagliatelle.rendering.LocalRenderContext;

import javax.annotation.Nonnull;
import java.util.List;

/**
 * Helper-macro to check if calls to deprecated macros are detected.
 */
@Register
@Deprecated
public class DeprecatedMacro implements Macro {

    @Override
    public Class<?> getType() {
        return String.class;
    }

    @Override
    public void verifyArguments(CompilationContext context, Position pos, List<Expression> args) {

    }

    @Override
    public Object eval(LocalRenderContext ctx, Expression[] args) {
        return "deprecated";
    }

    @Override
    public boolean isConstant(Expression[] args) {
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
