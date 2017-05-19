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
public class ConstantString extends Expression {

    public static final ConstantString EMPTY_STRING = new ConstantString("");

    private String value;

    public ConstantString(String value) {
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
        return String.class;
    }

    @Override
    public String toString() {
        return value;
    }

    public String getValue() {
        return value;
    }
}
