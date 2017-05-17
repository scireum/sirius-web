/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.expression;

import sirius.tagliatelle.LocalRenderContext;

/**
 * Created by aha on 11.05.17.
 */
public class ReadGlobal extends Expression {

    private int index;
    private Class<?> type;

    public ReadGlobal(Class<?> type, int index) {
        this.type = type;
        this.index = index;
    }

    @Override
    public Object eval(LocalRenderContext ctx) {
        return ctx.getGlobal(index);
    }

    @Override
    public Class<?> getType() {
        return type;
    }

    @Override
    public String toString() {
        return "GLOBAL<" + index + ">";
    }
}
