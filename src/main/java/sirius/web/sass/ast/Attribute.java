/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.sass.ast;

/**
 * Represents an attribute within a section like:
 * <pre>
 * {@code
 *    font-size: 15px;
 * }
 * </pre>
 */
public class Attribute {
    private final String name;
    private Expression expression;

    /**
     * Creates a new attribute with the given name
     *
     * @param name the name of the attribute without ":" at the end
     */
    public Attribute(String name) {
        this.name = name;
    }

    /**
     * Sets the expression representing the value of the attribute
     *
     * @param expression the new value of the attribute
     */
    public void setExpression(Expression expression) {
        this.expression = expression;
    }

    /**
     * Returns the expression representing the value of the attribute
     *
     * @return the value of the attribute as expression
     */
    public Expression getExpression() {
        return expression;
    }

    @Override
    public String toString() {
        return name + ": " + expression + ";";
    }

    /**
     * Returns the name of the attribute without the trailing ":"
     *
     * @return the name of the attribute
     */
    public String getName() {
        return name;
    }
}
