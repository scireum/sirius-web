/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.expression;

/**
 * Represents a constant expression.
 */
public abstract class ConstantExpression extends Expression {

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
        return true;
    }

    @Override
    public Expression copy() {
        return this;
    }
}
