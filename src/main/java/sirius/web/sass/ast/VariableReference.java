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
 * References a variable like "$test".
 */
public class VariableReference implements Expression {
    private String name;

    /**
     * Creates a new reference for the given variable.
     *
     * @param name the name of the variable to reference (without $).
     */
    public VariableReference(String name) {
        this.name = name;
    }

    /**
     * Returns the name of the variable.
     *
     * @return the name of the variable (without $)
     */
    public String getName() {
        return name;
    }

    @Override
    public String toString() {
        return "$" + name;
    }

    @Override
    public boolean isConstant() {
        return false;
    }

    @Override
    public Expression eval(Scope scope, Generator gen) {
        return scope.get(name).eval(scope, gen);
    }
}
