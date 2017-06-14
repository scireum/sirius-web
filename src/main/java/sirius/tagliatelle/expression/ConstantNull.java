/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.expression;

import sirius.tagliatelle.rendering.LocalRenderContext;

/**
 * Represents <tt>null</tt> as expression.
 */
public class ConstantNull extends ConstantExpression {

    /**
     * Represents <tt>null</tt> as expression
     */
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
