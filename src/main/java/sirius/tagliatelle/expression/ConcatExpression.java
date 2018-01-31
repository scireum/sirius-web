/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.expression;

import sirius.tagliatelle.rendering.LocalRenderContext;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Resprents a string concatenation operation.
 */
public class ConcatExpression implements Expression {

    private List<Expression> stringExpressions = new ArrayList<>();

    /**
     * Creates a new string concatenation for the given expressions
     *
     * @param expressions the list of expressions to evaluate and concatenate.
     */
    public ConcatExpression(Expression... expressions) {
        stringExpressions.addAll(Arrays.asList(expressions));
    }

    public void add(Expression expr) {
        stringExpressions.add(expr);
    }

    @Override
    public Expression copy() {
        ConcatExpression copy = new ConcatExpression();
        for (Expression expr : stringExpressions) {
            copy.stringExpressions.add(expr.copy());
        }
        return copy;
    }

    @Override
    public Expression propagateVisitor(ExpressionVisitor visitor) {
        for (int i = 0; i < stringExpressions.size(); i++) {
            stringExpressions.set(i, stringExpressions.get(i).propagateVisitor(visitor));
        }
        return visitor.visitThis(this);
    }

    /**
     * Tries to optimize the expression by pre-concatenating adjacent constant operands.
     *
     * @return an optimized (if possible) expression with a minimal number of constant operands
     */
    @Override
    public Expression reduce() {
        StringBuilder sb = null;
        List<Expression> expressions = stringExpressions.stream().map(Expression::reduce).collect(Collectors.toList());
        stringExpressions.clear();
        for (Expression expression : expressions) {
            sb = reduceExpression(sb, expression);
        }

        if (sb != null) {
            if (stringExpressions.isEmpty()) {
                return new ConstantString(sb.toString());
            }

            stringExpressions.add(new ConstantString(sb.toString()));
        }

        return this;
    }

    private StringBuilder reduceExpression(StringBuilder currentBuilder, Expression expression) {
        if (expression.isConstant()) {
            Object result = expression.eval(null);
            if (result == null) {
                return currentBuilder;
            }
            if (currentBuilder != null) {
                return currentBuilder.append(result);
            }
            return new StringBuilder().append(result);
        }

        if (currentBuilder != null) {
            stringExpressions.add(new ConstantString(currentBuilder.toString()));
        }

        stringExpressions.add(expression);
        return null;
    }

    @Override
    public boolean isConstant() {
        return false;
    }

    @Override
    public Object eval(LocalRenderContext ctx) {
        StringBuilder sb = new StringBuilder();
        for (Expression expr : stringExpressions) {
            Object result = expr.eval(ctx);
            if (result != null) {
                sb.append(result);
            }
        }
        return sb.toString();
    }

    @Override
    public Class<?> getType() {
        return String.class;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        for (Expression expr : stringExpressions) {
            if (sb.length() > 0) {
                sb.append(" + ");
            }
            sb.append(expr);
        }

        return sb.toString();
    }
}
