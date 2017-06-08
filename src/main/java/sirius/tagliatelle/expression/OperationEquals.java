/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.expression;

import sirius.tagliatelle.rendering.LocalRenderContext;

import java.util.Objects;

/**
 * Creates a <tt>==</tt> (in terms of {@link Object#equals(Object)}) operation.
 * <p>
 * Note that this can also be inverted to <tt>!=</tt>.
 */
public class OperationEquals extends Expression {

    protected Expression leftExpression;
    protected Expression rightExpression;
    protected boolean invert;

    /**
     * Creates a new operation for the given operands.
     * @param leftExpression the left operand
     * @param rightExpression the right operand
     * @param invert uses <tt>!=</tt> as operator if <tt>true</tt>, <tt>==</tt> otherwise.
     */
    public OperationEquals(Expression leftExpression, Expression rightExpression, boolean invert) {
        this.leftExpression = leftExpression;
        this.rightExpression = rightExpression;
        this.invert = invert;
    }

    @Override
    public Expression visit(ExpressionVisitor visitor) {
        this.leftExpression = leftExpression.visit(visitor);
        this.rightExpression = rightExpression.visit(visitor);
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
