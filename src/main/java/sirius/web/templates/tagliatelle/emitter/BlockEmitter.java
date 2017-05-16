/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates.tagliatelle.emitter;

import sirius.web.templates.tagliatelle.LocalRenderContext;

/**
 * Created by aha on 16.05.17.
 */
public class BlockEmitter implements Emitter {

    private String name;

    public BlockEmitter(String name) {
        this.name = name;
    }

    @Override
    public void emit(LocalRenderContext context) {
        context.emitBlock(name);
    }

}
