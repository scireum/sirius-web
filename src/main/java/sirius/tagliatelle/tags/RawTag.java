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
import sirius.tagliatelle.emitter.RawEmitter;

import javax.annotation.Nonnull;

/**
 * Handles <tt>i:raw</tt> which sets the <tt>escaper</tt> to
 * {@link sirius.tagliatelle.rendering.GlobalRenderContext#escapeRAW(String)} while emitting its body.
 */
public class RawTag extends TagHandler {

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
