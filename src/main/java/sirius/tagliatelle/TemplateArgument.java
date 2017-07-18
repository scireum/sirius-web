/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle;

import sirius.tagliatelle.expression.Expression;

import javax.annotation.Nullable;

/**
 * Represents an argument definition for a template.
 */
public class TemplateArgument {

    private final String description;
    private final Class<?> type;
    private final String name;
    private final Expression defaultValue;

    /**
     * Creates a new argument with the given type, name and default expression.
     *
     * @param type         the type of the argument
     * @param name         the name of the argument
     * @param description  a description of the argument
     * @param defaultValue the expression which yields the default value if no argument is supplied.
     */
    public TemplateArgument(Class<?> type,
                            String name,
                            @Nullable String description,
                            @Nullable Expression defaultValue) {
        this.type = type;
        this.name = name;
        this.description = description;
        this.defaultValue = defaultValue;
    }

    /**
     * Returns the type of the argument.
     *
     * @return the type of the argument
     */
    public Class<?> getType() {
        return type;
    }

    /**
     * Returns the name of the argument.
     *
     * @return the name of the argument
     */
    public String getName() {
        return name;
    }

    /**
     * Returns the default value.
     *
     * @return the expression which yields the default value if not argument is given
     */
    public Expression getDefaultValue() {
        return defaultValue;
    }

    /**
     * Returns the description of the argument.
     *
     * @return a short description of the argument
     */
    public String getDescription() {
        return description;
    }
}
