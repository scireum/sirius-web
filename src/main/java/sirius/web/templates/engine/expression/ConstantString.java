/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates.engine.expression;

import sirius.web.templates.engine.RenderContext;

/**
 * Created by aha on 10.05.17.
 */
public class ConstantString extends Expression {

    public static final ConstantString EMPTY_STRING = new ConstantString("");

    private String value;

    public ConstantString(String value) {
        this.value = value;
    }

    @Override
    public Object eval(RenderContext ctx) {
        return value;
    }

    @Override
    public Class<?> getType() {
        return String.class;
    }

    @Override
    public String toString() {
        return value;
    }

    public String getValue() {
        return value;
    }
}
