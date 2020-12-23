/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.tagliatelle.emitter;

import parsii.tokenizer.Position;
import sirius.pasta.tagliatelle.rendering.GlobalRenderContext;
import sirius.pasta.tagliatelle.rendering.LocalRenderContext;

import java.util.function.UnaryOperator;

/**
 * Switches the <tt>escaper</tt> to {@link GlobalRenderContext#escapeRAW(String)} while emitting the given body.
 */
public class RawEmitter extends Emitter {

    private Emitter body;

    /**
     * Creates a new instance for the given position and body.
     *
     * @param startOfBlock the position where the emitter was created
     * @param body         the inner body to output without any escaping
     */
    public RawEmitter(Position startOfBlock, Emitter body) {
        super(startOfBlock);
        this.body = body;
    }

    @Override
    protected void emitToContext(LocalRenderContext context) throws Exception {
        UnaryOperator<String> currentEscaper = context.getGlobalContext().getEscaper();
        context.getGlobalContext().setEscaper(GlobalRenderContext::escapeRAW);
        try {
            body.emit(context);
        } finally {
            context.getGlobalContext().setEscaper(currentEscaper);
        }
    }

    @Override
    public Emitter reduce() {
        if (body != null) {
            body = body.reduce();
        }

        return this;
    }
}
