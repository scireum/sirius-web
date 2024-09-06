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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Represents a list of values.
 */
public class ValueList implements Expression {

    private final List<Expression> elements = new ArrayList<>();
    private boolean keepCommas = false;

    /**
     * Creates a new and empty value list
     *
     * @param keepCommas determines if commas are kept in the output or not
     */
    public ValueList(boolean keepCommas) {
        this.keepCommas = keepCommas;
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        for (Expression expression : elements) {
            if (builder.length() > 0) {
                builder.append(keepCommas ? "," : " ");
            }
            builder.append(expression);
        }
        return builder.toString();
    }

    /**
     * Adds the given element to the list.
     *
     * @param element the element to add
     */
    public void add(Expression element) {
        elements.add(element);
    }

    /**
     * Returns the contents of the value list.
     *
     * @return a list of all elements in the value list
     */
    public List<Expression> getElements() {
        return Collections.unmodifiableList(elements);
    }

    @Override
    public boolean isConstant() {
        for (Expression expression : elements) {
            if (!expression.isConstant()) {
                return false;
            }
        }
        return true;
    }

    @Override
    public Expression eval(Scope scope, Generator generator) {
        ValueList result = new ValueList(keepCommas);
        for (Expression expression : elements) {
            result.elements.add(expression.eval(scope, generator));
        }
        return result;
    }
}
