/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates.tagliatelle;

import sirius.web.templates.tagliatelle.expression.Expression;

/**
 * Created by aha on 16.05.17.
 */
public class TemplateArgument {
    private Class<?> type;
    private String name;
    private Expression defaultValue;

    public TemplateArgument(Class<?> type, String name, Expression defaultValue) {
        this.type = type;
        this.name = name;
        this.defaultValue = defaultValue;
    }

    public Class<?> getType() {
        return type;
    }

    public String getName() {
        return name;
    }

    public Expression getDefaultValue() {
        return defaultValue;
    }
}
