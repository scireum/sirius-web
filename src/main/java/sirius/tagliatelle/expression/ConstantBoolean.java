/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.expression;

import sirius.tagliatelle.LocalRenderContext;

/**
 * Created by aha on 10.05.17.
 */
public class ConstantBoolean extends Expression {

    public static final ConstantBoolean TRUE = new ConstantBoolean(true);
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
    public Expression visit(ExpressionVisitor visitor) {
        return visitor.visit(this);
    }

    @Override
    public Expression reduce() {
        return this;
    }

    @Override
    public boolean isConstant() {
        return true;
    }

    @Override
    public Expression copy() {
        return this;
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
