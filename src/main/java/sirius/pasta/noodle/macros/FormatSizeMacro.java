/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.macros;

import sirius.kernel.tokenizer.Position;
import sirius.kernel.di.std.Register;
import sirius.kernel.nls.NLS;
import sirius.pasta.noodle.Environment;
import sirius.pasta.noodle.compiler.CompilationContext;
import sirius.pasta.noodle.compiler.ir.Node;
import sirius.pasta.noodle.sandbox.PublicApi;

import javax.annotation.Nonnull;
import java.util.List;

/**
 * Represents <tt>formatSize(long)</tt> which is a call to {@link NLS#formatSize(long)}.
 */
@Register
@PublicApi
public class FormatSizeMacro extends BasicMacro {
    @Override
    public Class<?> getType() {
        return String.class;
    }

    @Override
    public void verifyArguments(CompilationContext context, Position position, List<Class<?>> args) {
        if (args.size() != 1 && !CompilationContext.isAssignableTo(args.get(0), Long.class)) {
            throw new IllegalArgumentException("One parameter is expected");
        }
    }

    @Override
    public Object invoke(Environment environment, Object[] args) {
        Long size = (Long) args[0];
        if (size == null) {
            return "";
        }

        return NLS.formatSize(size);
    }

    @Override
    public boolean isConstant(CompilationContext context, List<Node> args) {
        return false;
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
