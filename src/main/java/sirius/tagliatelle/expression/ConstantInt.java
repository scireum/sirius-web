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
 * Created by aha on 10.05.17.
 */
public class ConstantInt extends ConstantExpression {

    public static final ConstantInt ZERO = new ConstantInt(0);
    public static final ConstantInt ONE = new ConstantInt(1);
    public static final ConstantInt MINUS_ONE = new ConstantInt(-1);

    private int value;

    public ConstantInt(int value) {
        this.value = value;
    }

    @Override
    public Object eval(LocalRenderContext ctx) {
        return value;
    }

    @Override
    public Class<?> getType() {
        return int.class;
    }

    @Override
    public String toString() {
        return String.valueOf(value);
    }
}
