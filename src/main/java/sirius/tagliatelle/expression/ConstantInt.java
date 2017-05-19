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
public class ConstantInt extends Expression {

    public static final ConstantInt ZERO = new ConstantInt(0);
    public static final ConstantInt ONE = new ConstantInt(1);
    public static final ConstantInt MINUS_ONE = new ConstantInt(-1);

    private int value;

    public ConstantInt(int value) {
        this.value = value;
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
