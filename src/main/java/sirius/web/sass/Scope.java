/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.sass;

import sirius.web.sass.ast.Expression;
import sirius.web.sass.ast.Value;

import java.util.Map;
import java.util.TreeMap;

/**
 * Represents a map which binds values to variable names.
 * <p>
 * To support nested contexts of mixins, a parent scope can be given, which will be used to resolve unknown
 * variables.
 */
public class Scope {

    private Scope parent;
    private final Map<String, Expression> variables = new TreeMap<>();

    /**
     * Creates a new and empty scope without a parent scope
     */
    public Scope() {
    }

    /**
     * Creates a new and empty scope using the given parameter as parent scope.
     *
     * @param scope the parent scope to use
     */
    public Scope(Scope scope) {
        this.parent = scope;
    }

    /**
     * Sets a variable in this scope. If a variable with the same name exists in the parent scope, this will be
     * shadowed (no longer reachable) but not changed.
     *
     * @param name  the name of the variable to set
     * @param value the value of the variable to set
     */
    public void set(String name, Expression value) {
        variables.put(name, value);
    }

    /**
     * Returns the value previously set for the given variable.
     *
     * @param name the variable to lookup
     * @return the value associated with the given name. Uses the parent scope if no variable with the given
     * name exists. Returns a {@link Value} with "" as content, in case the value is completely unknown.
     */
    public Expression get(String name) {
        if (variables.containsKey(name)) {
            return variables.get(name);
        }
        if (parent == null) {
            return new Value("");
        }
        return parent.get(name);
    }

    /**
     * Determines if this scope has a value set for the given variable. The parent scope will not be checked.
     *
     * @param name the variable to lookup
     * @return <tt>true</tt> if a variable with the given name exists in this scope, <tt>false</tt> otherwise.
     */
    public boolean has(String name) {
        return variables.containsKey(name);
    }
}
