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
 * Represents a constant boolean value.
 */
public class ConstantBoolean extends ConstantExpression {

    /**
     * Represents {@link Boolean#TRUE}.
     */
    public static final ConstantBoolean TRUE = new ConstantBoolean(true);

    /**
     * Represents {@link Boolean#FALSE}.
     */
    public static final ConstantBoolean FALSE = new ConstantBoolean(false);

    private boolean value;

    private ConstantBoolean(boolean value) {
        this.value = value;
    }

    @Override
    public Object eval(LocalRenderContext ctx) {
        return value;
    }

    @Override
    public Class<?> getType() {
        return boolean.class;
    }

    @Override
    public String toString() {
        return String.valueOf(value);
    }
}
