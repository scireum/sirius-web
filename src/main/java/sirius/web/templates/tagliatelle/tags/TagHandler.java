/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates.tagliatelle.tags;

import sirius.kernel.commons.Value;
import sirius.web.templates.tagliatelle.TagContext;
import sirius.web.templates.tagliatelle.emitter.Emitter;
import sirius.web.templates.tagliatelle.expression.ConstantString;
import sirius.web.templates.tagliatelle.expression.Expression;

import java.util.HashMap;
import java.util.Map;

/**
 * Created by aha on 12.05.17.
 */
public abstract class TagHandler {

    protected Map<String, Emitter> blocks = null;
    protected Map<String, Expression> attributes = null;

    public void addBlock(String name, Emitter body) {
        if (blocks == null) {
            blocks = new HashMap<>();
        }
        //TODO warn for duplicate names
        blocks.put(name, body);
    }

    public Emitter getBlock(String name) {
        if (blocks == null) {
            return null;
        }

        return blocks.get(name);
    }

    public Expression getAttribute(String name) {
        if (attributes == null) {
            return null;
        }

        return attributes.get(name);
    }

    public Value getConstantAttribute(String name) {
        if (attributes == null) {
            return Value.EMPTY;
        }

        Expression expr = attributes.get(name);
        if (expr == null) {
            return Value.EMPTY;
        }
        if (!(expr instanceof ConstantString)) {
            //TODO throw
        }

        return Value.of(((ConstantString) expr).getValue());
    }

    public abstract void apply(TagContext context);

    public Class<?> getExpectedAttributeType(String name) {
        return null;
    }

    public void setAttribute(String name, Expression expression) {
        if (attributes == null) {
            attributes = new HashMap<>();
        }
        attributes.put(name, expression);
    }
}
