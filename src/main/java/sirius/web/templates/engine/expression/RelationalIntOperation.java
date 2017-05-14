/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates.engine.expression;

import sirius.web.templates.engine.RenderContext;

/**
 * Created by aha on 12.05.17.
 */
public class RelationalIntOperation extends Expression {

    protected Expression leftExpression;
    protected Expression rightExpression;
    protected Operator operator;

    public RelationalIntOperation(Operator operator, Expression leftExpression, Expression rightExpression) {
        this.operator = operator;
        this.leftExpression = leftExpression;
        this.rightExpression = rightExpression;
        //TODO ensure int
    }

    @Override
    public Object eval(RenderContext ctx) {
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
                //TODO
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
