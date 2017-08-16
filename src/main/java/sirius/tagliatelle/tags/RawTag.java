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
import sirius.tagliatelle.emitter.RawEmitter;

import javax.annotation.Nonnull;
import java.util.Collections;
import java.util.List;

/**
 * Handles <tt>i:raw</tt> which sets the <tt>escaper</tt> to
 * {@link sirius.tagliatelle.rendering.GlobalRenderContext#escapeRAW(String)} while emitting its body.
 */
public class RawTag extends TagHandler {

    /**
     * Creates new tags of the given type (name).
     */
    @Register
    public static class Factory implements TagHandlerFactory {

        @Nonnull
        @Override
        public String getName() {
            return "i:raw";
        }

        @Override
        public TagHandler createHandler() {
            return new RawTag();
        }

        @Override
        public List<TemplateArgument> reportArguments() {
            return Collections.emptyList();
        }

        @Override
        public String getDescription() {
            return "Disables all output filtering (e.g. HTML escaping) and outputs all strings as given.";
        }
    }

    @Override
    public void apply(CompositeEmitter targetBlock) {
        Emitter body = getBlock("body");
        if (body != null) {
            targetBlock.addChild(new RawEmitter(getStartOfTag(), body));
        } else {
            compilationContext.error(getStartOfTag(), "A raw tag should have a body!");
        }
    }
}
