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
 * Created by aha on 12.05.17.
 */
public class IntOperation extends Expression {
    protected Expression leftExpression;
    protected Expression rightExpression;
    protected Operator operator;

    public IntOperation(Operator operator, Expression leftExpression, Expression rightExpression) {
        this.operator = operator;
        this.leftExpression = leftExpression;
        this.rightExpression = rightExpression;
        //TODO ensure int
    }

    @Override
    public Object eval(LocalRenderContext ctx) {
        int left = (int) leftExpression.eval(ctx);
        int right = (int) rightExpression.eval(ctx);

        switch (operator) {
            case ADD:
                return left + right;
            case SUBTRACT:
                return left - right;
            case MULTIPLY:
                return left * right;
            case DIVIDE:
                return left / right;
            case MODULO:
                return left % right;
            default:
                //TODO
                throw new IllegalArgumentException(operator.toString());
        }
    }

    @Override
    public Class<?> getType() {
        return int.class;
    }

    @Override
    public String toString() {
        return leftExpression + " " + operator + " " + rightExpression;
    }
}
