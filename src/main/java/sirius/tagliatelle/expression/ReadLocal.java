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
 * Performs a read on the local stack.
 *
 * @see LocalRenderContext#getLocal(int)
 */
public class ReadLocal implements Expression {

    private int index;
    private Class<?> type;

    /**
     * Creates a new instance which reads the given index in the local stack.
     *
     * @param type  the expected type of the value being read
     * @param index the index to read
     */
    public ReadLocal(Class<?> type, Integer index) {
        this.type = type;
        this.index = index;
    }

    @Override
    public Expression propagateVisitor(ExpressionVisitor visitor) {
        return visitor.visitThis(this);
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
        return ctx.getLocal(index);
    }

    @Override
    public Class<?> getType() {
        return type;
    }

    public int getIndex() {
        return index;
    }

    @Override
    public String toString() {
        return "LOCAL<" + index + ">";
    }
}
