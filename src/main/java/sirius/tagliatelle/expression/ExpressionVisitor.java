/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.expression;

import javax.annotation.Nonnull;

/**
 * Visits each node of an expression AST with the possibility to change or replace it.
 */
public interface ExpressionVisitor {

    /**
     * Invoked for each node of an expression AST.
     *
     * @param expression the expression to visit
     * @return either the given expression or a replacement node.
     */
    @Nonnull
    Expression visitThis(@Nonnull Expression expression);
}
