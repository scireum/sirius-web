/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.tags;

import sirius.kernel.di.std.Register;
import sirius.tagliatelle.emitter.CompositeEmitter;
import sirius.tagliatelle.emitter.Emitter;

import javax.annotation.Nonnull;

/**
 * Handles <tt>i:else</tt> within an <tt>i:if</tt>.
 */
public class TagElse extends TagHandler {

    @Register
    public static class Factory implements TagHandlerFactory {

        @Nonnull
        @Override
        public String getName() {
            return "i:else";
        }

        @Override
        public TagHandler createHandler() {
            return new TagElse();
        }
    }

    @Override
    public void apply(CompositeEmitter targetBlock) {
        if (!(getParentHandler() instanceof TagIf)) {
            getCompilationContext().error(getStartOfTag(), "i:else must be defined within i:if!");
            return;
        }
        Emitter body = getBlock("body");
        if (body != null) {
            getParentHandler().addBlock("else", body);
        }
    }
}
