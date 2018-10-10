/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.tags;

import sirius.kernel.di.std.Register;
import sirius.tagliatelle.TemplateArgument;
import sirius.tagliatelle.emitter.CompositeEmitter;
import sirius.tagliatelle.emitter.Emitter;

import javax.annotation.Nonnull;
import java.util.Collections;
import java.util.List;

/**
 * Handles <tt>i:else</tt> within an <tt>i:if</tt>.
 */
public class ElseTag extends TagHandler {

    /**
     * Creates new tags of the given type (name).
     */
    @Register
    public static class Factory implements TagHandlerFactory {

        @Nonnull
        @Override
        public String getName() {
            return "i:else";
        }

        @Override
        public TagHandler createHandler() {
            return new ElseTag();
        }

        @Override
        public List<TemplateArgument> reportArguments() {
            return Collections.emptyList();
        }

        @Override
        public String getDescription() {
            return "Provides an else part for an i:if tag.";
        }
    }

    @Override
    public void beforeBody() {
        if (!checkParentHandler()) {
            return;
        }

        ((IfTag) getParentHandler()).clearLocalsFromStack();
    }

    @Override
    public void apply(CompositeEmitter targetBlock) {
        if (!checkParentHandler()) {
            return;
        }

        Emitter body = getBlock("body");
        if (body != null) {
            getParentHandler().addBlock("else", body);
        }
    }

    private boolean checkParentHandler() {
        if (!(getParentHandler() instanceof IfTag)) {
            getCompilationContext().error(getStartOfTag(), "i:else must be defined within i:if!");
            return false;
        }

        return true;
    }
}
