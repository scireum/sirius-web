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
 * Performs a read in the environment list.
 *
 * @see LocalRenderContext#getGlobal
 */
public class ReadGlobal extends Expression {

    private int index;
    private Class<?> type;

    /**
     * Creates a new instance which reads the given index in the environment list (aka global variable).
     *
     * @param type  the expected type of the value being read
     * @param index the index to read
     */
    public ReadGlobal(Class<?> type, int index) {
        this.type = type;
        this.index = index;
    }

    @Override
    public Expression visit(ExpressionVisitor visitor) {
        return visitor.visit(this);
    }

    @Override
    public Expression reduce() {
        return this;
    }

    @Override
    public boolean isConstant() {
        return false;
    }

    @Override
    public Expression copy() {
        return this;
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
