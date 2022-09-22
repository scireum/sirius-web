/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.sass.ast;

import sirius.web.sass.Generator;
import sirius.web.sass.Scope;

/**
 * Represents an attribute filter used in a media query like "(min-width: 13px)".
 */
public class MediaFilter implements Expression {
    private String name;
    private Expression expression;

    /**
     * Creates a new media filter for the given attribute (without ":").
     *
     * @param name the name of the attribute to filter on
     */
    public MediaFilter(String name) {
        this.name = name;
    }

    /**
     * Sets the filter expression
     *
     * @param expression the filter expression to set
     */
    public void setExpression(Expression expression) {
        this.expression = expression;
    }

    /**
     * Returns the filter expression.
     *
     * @return the filter expression previously set
     */
    public Expression getExpression() {
        return expression;
    }

    @Override
    public String toString() {
        return "(" + name + ": " + expression + ")";
    }

    /**
     * Returns the name of the attribute being filtered on
     *
     * @return the name of the filtered attribute
     */
    public String getName() {
        return name;
    }

    @Override
    public boolean isConstant() {
        return expression.isConstant();
    }

    @Override
    public Expression eval(Scope scope, Generator gen) {
        MediaFilter result = new MediaFilter(name);
        result.setExpression(expression.eval(scope, gen));
        return result;
    }
}
