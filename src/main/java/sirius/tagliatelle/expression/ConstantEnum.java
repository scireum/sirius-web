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
 * Represents an enum constant.
 */
public class ConstantEnum extends ConstantExpression {

    private final Class<?> type;
    private final Object value;

    /**
     * Creates a new instance representing the given class and enum constant.
     *
     * @param type  the class to represent
     * @param value the value to represent
     */
    public ConstantEnum(Class<?> type, Object value) {
        this.type = type;
        this.value = value;
    }

    @Override
    public Object eval(LocalRenderContext ctx) {
        return value;
    }

    @Override
    public Class<?> getType() {
        return type;
    }

    @Override
    public String toString() {
        return value.toString();
    }
}
