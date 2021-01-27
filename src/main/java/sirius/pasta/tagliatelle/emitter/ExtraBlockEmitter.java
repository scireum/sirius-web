/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.tagliatelle.emitter;

import sirius.pasta.tagliatelle.rendering.LocalRenderContext;

import javax.annotation.Nonnull;

/**
 * Emits the contents of an i:block defined at top-level into an extra block output of the context.
 * <p>
 * This permits to render several strings and sections within a single template.
 *
 * @see sirius.pasta.tagliatelle.rendering.GlobalRenderContext#getExtraBlock(String)
 */
public class ExtraBlockEmitter extends Emitter {

    private final String name;
    private Emitter body;

    /**
     * Creates a new instance with the given name and body.
     *
     * @param name the name of the block
     * @param body the body of the block
     */
    public ExtraBlockEmitter(String name, Emitter body) {
        super(body.startOfBlock);
        this.name = name;
        this.body = body;
    }

    @Override
    protected void emitToContext(@Nonnull LocalRenderContext context) throws Exception {
        context.getGlobalContext().storeExtraBlock(name, context.getGlobalContext().emitToString(() -> {
            body.emit(context);
        }));
    }

    @Nonnull
    @Override
    public Emitter reduce() {
        body = body.reduce();
        return this;
    }
}
