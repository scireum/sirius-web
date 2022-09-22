/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.sass.ast;

/**
 * Represents a variable definition like "$test: 'Hello'"
 */
public class Variable {

    private Expression value;
    private boolean defaultValue = false;
    private String name;

    /**
     * Sets the name of the variable (without $).
     *
     * @param name the name of the variable
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * Returns the name of the variable.
     *
     * @return the name of the variable without $.
     */
    public String getName() {
        return name;
    }

    /**
     * Returns the value of the variable
     *
     * @return the expression defining the variable
     */
    public Expression getValue() {
        return value;
    }

    /**
     * Sets the value of the variable.
     *
     * @param value the expression defining the variable
     */
    public void setValue(Expression value) {
        this.value = value;
    }

    /**
     * Defines if the given value is a default only.
     *
     * @return <tt>true</tt> if the variable was defined using "!default"
     */
    public boolean isDefaultValue() {
        return defaultValue;
    }

    /**
     * Sets if the variable was defined using "!default".
     *
     * @param defaultValue sets the default flag of the variable
     */
    public void setDefaultValue(boolean defaultValue) {
        this.defaultValue = defaultValue;
    }

    @Override
    public String toString() {
        return name + ": " + value + (defaultValue ? " !default" : "");
    }
}
