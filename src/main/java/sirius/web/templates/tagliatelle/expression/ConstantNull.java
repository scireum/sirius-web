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
 * Created by aha on 10.05.17.
 */
public class ConstantNull extends Expression {

    public static final ConstantNull NULL = new ConstantNull();

    private ConstantNull() {
    }

    @Override
    public Object eval(LocalRenderContext ctx) {
        return null;
    }

    @Override
    public Class<?> getType() {
        return void.class;
    }

    @Override
    public String toString() {
        return "null";
    }
}
