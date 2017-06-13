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
 * Represents a constant string value.
 */
public class ConstantString extends ConstantExpression {

    /**
     * Represents <tt>""</tt> as expression.
     */
    public static final ConstantString EMPTY_STRING = new ConstantString("");

    private final String value;

    /**
     * Creates a new instance representing the given string.
     *
     * @param value the string to represent
     */
    public ConstantString(String value) {
        this.value = value;
    }

    @Override
    public Object eval(LocalRenderContext ctx) {
        return value;
    }

    @Override
    public Class<?> getType() {
        return String.class;
    }

    @Override
    public String toString() {
        return "'" + value.replace("'", "\\'") + "'";
    }

    /**
     * Returns the string represented by this expression.
     *
     * @return the string value
     */
    public String getValue() {
        return value;
    }
}
