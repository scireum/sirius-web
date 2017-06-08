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

/**
 * Created by aha on 07.06.17.
 */
public abstract class ExpressionHandler implements Priorized {

    @Override
    public int getPriority() {
        return Priorized.DEFAULT_PRIORITY;
    }

    public abstract boolean shouldProcess(Compiler compiler);

    public abstract Emitter process(Compiler compiler);
}
