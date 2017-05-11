/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates.engine.expression;

import sirius.web.templates.engine.RenderContext;

/**
 * Created by aha on 10.05.17.
 */
public class ConstantBoolean extends Expression {

    public static final ConstantBoolean TRUE = new ConstantBoolean(true);
    public static final ConstantBoolean FALSE = new ConstantBoolean(false);

    private boolean value;

    private ConstantBoolean(boolean value) {
        this.value = value;
    }

    @Override
    public Object eval(RenderContext ctx) {
        return value;
    }

    @Override
    public Class<?> getType() {
        return boolean.class;
    }
}
