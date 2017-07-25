/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.macros;

import sirius.kernel.di.std.Register;
import sirius.kernel.nls.NLS;
import sirius.tagliatelle.Tagliatelle;
import sirius.tagliatelle.expression.Expression;
import sirius.tagliatelle.rendering.LocalRenderContext;

import javax.annotation.Nonnull;
import java.util.List;

/**
 * Formats the given file or byte size as long to human readable size.
 *
 * @see sirius.kernel.nls.NLS#formatSize(long)
 */
@Register
public class FormatByteSizeMacro implements Macro {

    @Override
    public Class<?> getType() {
        return String.class;
    }

    @Override
    public void verifyArguments(List<Expression> args) {
        if (args.size() != 1 || !Tagliatelle.isAssignableTo(args.get(0).getType(), long.class)) {
            throw new IllegalArgumentException("Expected the parameter to be of primitive long type.");
        }
    }

    @Override
    public Object eval(LocalRenderContext ctx, Expression[] args) {
        return NLS.formatSize((long) args[0].eval(ctx));
    }

    @Override
    public boolean isConstant(Expression[] args) {
        return true;
    }

    @Override
    public String getDescription() {
        return "Formats file or byte size to human readable size.";
    }

    @Nonnull
    @Override
    public String getName() {
        return "formatByteSize";
    }
}
