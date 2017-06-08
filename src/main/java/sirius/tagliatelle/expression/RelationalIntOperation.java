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
 * Represents a relational operation on integer values.
 */
public class RelationalIntOperation extends Expression {

    protected Expression leftExpression;
    protected Expression rightExpression;
    protected Operator operator;

    /**
     * Creates a new instance for the given operator and operands.
     *
     * @param operator        the operator to use
     * @param leftExpression  the left operand
     * @param rightExpression the right operand
     */
    public RelationalIntOperation(Operator operator, Expression leftExpression, Expression rightExpression) {
        this.operator = operator;
        this.leftExpression = leftExpression;
        this.rightExpression = rightExpression;
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

        if (leftExpression instanceof ConstantInt && rightExpression instanceof ConstantInt) {
            boolean result = (boolean) eval(null);
            if (result) {
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
        return new RelationalIntOperation(operator, leftExpression.copy(), rightExpression.copy());
    }

    @Override
    public Object eval(LocalRenderContext ctx) {
        int left = (int) leftExpression.eval(ctx);
        int right = (int) rightExpression.eval(ctx);

        switch (operator) {
            case LT:
                return left < right;
            case LT_EQ:
                return left <= right;
            case EQ:
                return left == right;
            case GT_EQ:
                return left >= right;
            case GT:
                return left > right;
            case NE:
                return left != right;
            default:
                throw new IllegalArgumentException(operator.toString());
        }
    }

    @Override
    public Class<?> getType() {
        return boolean.class;
    }

    @Override
    public String toString() {
        return leftExpression + " " + operator + " " + rightExpression;
    }
}
