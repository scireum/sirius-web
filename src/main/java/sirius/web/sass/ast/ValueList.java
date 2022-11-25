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
        StringBuilder sb = new StringBuilder();
        for (sirius.web.sass.ast.Expression expr : elements) {
            if (sb.length() > 0) {
                sb.append(keepCommas ? "," : " ");
            }
            sb.append(expr);
        }
        return sb.toString();
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
        for (sirius.web.sass.ast.Expression expr : elements) {
            if (!expr.isConstant()) {
                return false;
            }
        }
        return true;
    }

    @Override
    public sirius.web.sass.ast.Expression eval(Scope scope, Generator gen) {
        ValueList result = new ValueList(keepCommas);
        for (Expression expr : elements) {
            result.elements.add(expr.eval(scope, gen));
        }
        return result;
    }
}
