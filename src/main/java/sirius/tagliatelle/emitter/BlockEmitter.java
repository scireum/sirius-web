/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.emitter;

import parsii.tokenizer.Position;
import sirius.tagliatelle.LocalRenderContext;

/**
 * Created by aha on 16.05.17.
 */
public class BlockEmitter extends Emitter {

    private String name;

    public BlockEmitter(Position startOfBlock, String name) {
        super(startOfBlock);
        this.name = name;
    }

    @Override
    protected void emitToContext(LocalRenderContext context) throws Exception {
        context.emitBlock(name);
    }
}
