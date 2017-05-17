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

import java.util.ArrayList;
import java.util.List;

/**
 * Created by aha on 10.05.17.
 */
public class CompositeEmitter extends Emitter {

    private List<Emitter> children = new ArrayList<>();

    public CompositeEmitter(Position startOfBlock) {
        super(startOfBlock);
    }

    @Override
    protected void emitToContext(LocalRenderContext context) throws Exception {
        if (children != null) {
            for (Emitter expr : children) {
                expr.emit(context);
            }
        }
    }

    public void addChild(Emitter child) {
        children.add(child);
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        if (children != null) {
            for (Emitter expr : children) {
                sb.append(expr);
            }
        }
        return sb.toString();
    }
}
