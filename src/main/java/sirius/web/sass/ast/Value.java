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
 * Represents a plain value.
 */
public class Value implements Expression {
    private String contents;

    /**
     * Creates a new value representing the given contents a value.
     *
     * @param contents the value to be represented
     */
    public Value(String contents) {
        super();
        this.contents = contents;
    }

    /**
     * Returns the represented value
     *
     * @return the value to represent
     */
    public String getContents() {
        return contents;
    }

    @Override
    public String toString() {
        return contents;
    }

    @Override
    public boolean isConstant() {
        return true;
    }

    @Override
    public Expression eval(Scope scope, Generator gen) {
        return this;
    }
}
