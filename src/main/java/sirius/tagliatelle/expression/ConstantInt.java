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
 * Represents a constant integer number.
 */
public class ConstantInt extends ConstantExpression {

    /**
     * Represents <tt>0</tt>.
     */
    public static final ConstantInt ZERO = new ConstantInt(0);

    /**
     * Represents <tt>1</tt>.
     */
    public static final ConstantInt ONE = new ConstantInt(1);

    /**
     * Represents <tt>-1</tt>.
     */
    public static final ConstantInt MINUS_ONE = new ConstantInt(-1);

    private final int value;

    /**
     * Creates a new instance for the given number.
     *
     * @param value the number to represent
     */
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
