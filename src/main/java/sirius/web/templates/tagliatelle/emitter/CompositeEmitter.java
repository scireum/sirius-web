/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates.tagliatelle.emitter;

import sirius.web.templates.tagliatelle.LocalRenderContext;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by aha on 10.05.17.
 */
public class CompositeEmitter implements Emitter {

    private List<Emitter> children = new ArrayList<>();

    @Override
    public void emit(LocalRenderContext ctx) {
        if (children != null) {
            for (Emitter expr : children) {
                expr.emit(ctx);
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
