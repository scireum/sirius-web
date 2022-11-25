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
 * Base class for all AST classes
 */
public interface Expression {

    /**
     * Determines if this expression is constant or if it depends on variables.
     *
     * @return <tt>true</tt> if the expression is constant, <tt>false</tt> otherwise
     */
    boolean isConstant();

    /**
     * If possible the expression is evaluated and a simplified expression is returned.
     *
     * @param scope the scope used to resolve variables.
     * @param gen   the generator used to evaluate functions
     * @return a possibly simplified version of the expression
     */
    Expression eval(Scope scope, Generator gen);
}
