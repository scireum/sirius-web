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
 * Represents a node of the expression AST.
 */
public interface Expression {

    /**
     * Evaluates the expression into an object using the given context.
     *
     * @param ctx the current rendering context
     * @return the result of the evaluation
     */
    Object eval(LocalRenderContext ctx);

    /**
     * Visits all child nodes and then the node itself.
     *
     * @param visitor the visitor to invoke for all child nodes and on the node itself
     * @return the result of the invocation of the given visitor on this node
     */
    Expression propagateVisitor(ExpressionVisitor visitor);

    /**
     * Reduces and optimizes the expression if possible.
     *
     * @return either the node itself or an optimized / reduced version if possible
     */
    Expression reduce();

    /**
     * Determines if the expression is constant.
     *
     * @return <tt>true</tt> if the expression is constant, <tt>false</tt> otherwise
     */
    boolean isConstant();

    /**
     * Creates a deep copy of the expression which can be modified without affecting the original expression.
     *
     * @return a deep copy of the expression
     */
    Expression copy();

    /**
     * Returns the Java class of the objects yielded by this expression.
     *
     * @return the type of objects created by this expression
     */
    Class<?> getType();
}
