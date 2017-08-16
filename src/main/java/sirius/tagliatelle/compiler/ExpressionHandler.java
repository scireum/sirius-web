/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.compiler;

import sirius.kernel.di.std.Priorized;
import sirius.tagliatelle.emitter.Emitter;

import javax.annotation.Nullable;

/**
 * Subclasses can be {@link sirius.kernel.di.std.Register registered} and will be used to handle compiler expressions
 * starting with {@literal @}.
 */
public abstract class ExpressionHandler implements Priorized {

    @Override
    public int getPriority() {
        return Priorized.DEFAULT_PRIORITY;
    }

    /**
     * Determines if this handler is responsible, based on the current state of the reader.
     *
     * @param compiler the compiler which provides access to the {@link Compiler#getReader()} and {@link
     *                 Compiler#getContext()}.
     * @return <tt>true</tt> if this handler should parse the expression (the reader currently points to the
     * {@literal @} in the source code)
     */
    public abstract boolean shouldProcess(Compiler compiler);

    /**
     * Processes the expression initialized by an {@literal @} in the source code.
     *
     * @param compiler the compiler which provides access to the {@link Compiler#getReader()} and {@link
     *                 Compiler#getContext()}.
     * @return the emitter which represents the parsed source
     */
    @Nullable
    public abstract Emitter process(Compiler compiler);
}
