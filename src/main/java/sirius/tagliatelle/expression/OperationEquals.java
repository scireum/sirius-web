/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.expression;

import sirius.tagliatelle.LocalRenderContext;

import java.util.Objects;

/**
 * Created by aha on 12.05.17.
 */
public class OperationEquals extends Expression {

    protected Expression leftExpression;
    protected Expression rightExpression;
    protected boolean invert;

    public OperationEquals(Expression leftExpression, Expression rightExpression, boolean invert) {
        this.leftExpression = leftExpression;
        this.rightExpression = rightExpression;
        this.invert = invert;
    }

    @Override
    public Expression visit(ExpressionVisitor visitor) {
        this.leftExpression = visitor.visit(leftExpression);
        this.rightExpression = visitor.visit(rightExpression);
        return visitor.visit(this);
    }

    @Override
    public Expression reduce() {
        this.leftExpression = leftExpression.reduce();
        this.rightExpression = rightExpression.reduce();

        if (leftExpression.isConstant() && rightExpression.isConstant()) {
            boolean equal = Objects.equals(leftExpression.eval(null), rightExpression.eval(null));

            if (invert) {
                equal = !equal;
            }

            if (equal) {
                return ConstantBoolean.TRUE;
            } else {
                return ConstantBoolean.FALSE;
            }
        }

        return this;
    }

    @Override
    public boolean isConstant() {
        return false;
    }

    @Override
    public Expression copy() {
        return new OperationEquals(leftExpression.copy(), rightExpression.copy(), invert);
    }

    @Override
    public Object eval(LocalRenderContext ctx) {
        if (invert) {
            return !Objects.equals(leftExpression.eval(ctx), rightExpression.eval(ctx));
        } else {
            return Objects.equals(leftExpression.eval(ctx), rightExpression.eval(ctx));
        }
    }

    @Override
    public Class<?> getType() {
        return boolean.class;
    }

    @Override
    public String toString() {
        if (invert) {
            return leftExpression + " != " + rightExpression;
        }
        return leftExpression + " == " + rightExpression;
    }
}
