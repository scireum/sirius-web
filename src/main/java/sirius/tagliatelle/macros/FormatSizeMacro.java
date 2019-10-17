/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.macros;

import parsii.tokenizer.Position;
import sirius.kernel.di.std.Register;
import sirius.kernel.nls.NLS;
import sirius.tagliatelle.Tagliatelle;
import sirius.tagliatelle.compiler.CompilationContext;
import sirius.tagliatelle.expression.Expression;
import sirius.tagliatelle.rendering.LocalRenderContext;

import javax.annotation.Nonnull;
import java.util.List;

/**
 * Represents <tt>formatSize(long)</tt> which is a call to {@link NLS#formatSize(long)}.
 */
@Register
public class FormatSizeMacro implements Macro {
    @Override
    public Class<?> getType() {
        return String.class;
    }

    @Override
    public void verifyArguments(CompilationContext context, Position pos, List<Expression> args) {
        if (args.size() != 1 && !Tagliatelle.isAssignableTo(args.get(0).getType(), Long.class)) {
            throw new IllegalArgumentException("One parameter is expected");
        }
    }

    @Override
    public Object eval(LocalRenderContext ctx, Expression[] args) {
        Long size = (Long) args[0].eval(ctx);
        if (size == null) {
            return "";
        }

        return NLS.formatSize(size);
    }

    @Override
    public boolean isConstant(Expression[] args) {
        return true;
    }

    @Nonnull
    @Override
    public String getName() {
        return "formatSize";
    }

    @Override
    public String getDescription() {
        return "Formats the given parameter as size in bytes.";
    }
}
