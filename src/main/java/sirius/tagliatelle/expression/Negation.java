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
 * Represents a boolean <tt>and</tt>.
 */
public class Negation implements Expression {

    private Expression expression;

    public Negation(Expression expresion) {
        this.expression = expresion;
    }

    @Override
    public Expression reduce() {
        this.expression = expression.reduce();

        if (ConstantBoolean.TRUE.equals(expression)) {
            return ConstantBoolean.FALSE;
        }
        if (ConstantBoolean.FALSE.equals(expression)) {
            return ConstantBoolean.TRUE;
        }

        return this;
    }

    @Override
    public Expression propagateVisitor(ExpressionVisitor visitor) {
        this.expression = expression.propagateVisitor(visitor);
        return visitor.visitThis(this);
    }

    @Override
    public boolean isConstant() {
        return false;
    }

    @Override
    public Expression copy() {
        return new Negation(expression.copy());
    }

    @Override
    public Object eval(LocalRenderContext ctx) {
        return !(boolean) expression.eval(ctx);
    }

    @Override
    public Class<?> getType() {
        return boolean.class;
    }

    @Override
    public String toString() {
        return "!" + expression;
    }
}
