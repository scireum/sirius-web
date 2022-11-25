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
 * Represents a named parameter of a function call.
 * <p>
 * This is only used if the parameter is referenced by name - which is not supported by built-in functions.
 */
public class NamedParameter implements Expression {

    private String name;
    private Expression value;

    /**
     * Creates a new parameter for the given name and value.
     *
     * @param name  th name of the parameter.
     * @param value the value to be used for the parameter
     */
    public NamedParameter(String name, Expression value) {
        super();
        this.name = name;
        this.value = value;
    }

    @Override
    public String toString() {
        if (name != null) {
            return name + " = " + value.toString();
        } else {
            return value.toString();
        }
    }

    @Override
    public boolean isConstant() {
        return value.isConstant();
    }

    @Override
    public Expression eval(Scope scope, Generator gen) {
        if (isConstant()) {
            return this;
        }
        return new NamedParameter(name, value.eval(scope, gen));
    }
}
