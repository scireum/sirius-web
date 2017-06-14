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
 * Represents a constant class value
 */
public class ConstantClass extends ConstantExpression {

    private final Class<?> type;

    /**
     * Creates a new instance representing the given class.
     *
     * @param type the class to represent
     */
    public ConstantClass(Class<?> type) {
        this.type = type;
    }

    @Override
    public Object eval(LocalRenderContext ctx) {
        return type;
    }

    @Override
    public Class<?> getType() {
        return Class.class;
    }

    @Override
    public String toString() {
        return type.getName();
    }
}
