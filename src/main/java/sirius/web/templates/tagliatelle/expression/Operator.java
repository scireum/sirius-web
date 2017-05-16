/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates.tagliatelle.expression;

/**
 * Created by aha on 12.05.17.
 */
public enum Operator {
    LT("<"),

    LT_EQ("<="),

    EQ("=="),

    GT_EQ(">="),

    GT(">"),

    NE("!="),

    ADD("+"),

    SUBTRACT("-"),

    MULTIPLY("*"),

    DIVIDE("/"),

    MODULO("%");

    private final String name;

    Operator(String name) {
        this.name = name;
    }

    @Override
    public String toString() {
        return name;
    }
}
