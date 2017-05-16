/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates.tagliatelle.expression;

import sirius.web.templates.tagliatelle.LocalRenderContext;

/**
 * Created by aha on 11.05.17.
 */
public class ReadLocal extends Expression {

    private int index;
    private Class<?> type;

    public ReadLocal(Class<?> type, Integer index) {
        this.type = type;
        this.index = index;
    }

    @Override
    public Object eval(LocalRenderContext ctx) {
        return ctx.getLocal(index);
    }

    @Override
    public Class<?> getType() {
        return type;
    }

    @Override
    public String toString() {
        return "LOCAL<" + index + ">";
    }
}
