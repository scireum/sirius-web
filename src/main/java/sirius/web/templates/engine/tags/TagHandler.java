/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates.engine.tags;

import sirius.kernel.commons.Value;
import sirius.web.templates.engine.emitter.CompositeEmitter;
import sirius.web.templates.engine.expression.ConstantString;
import sirius.web.templates.engine.expression.Expression;

import java.util.HashMap;
import java.util.Map;

/**
 * Created by aha on 12.05.17.
 */
public abstract class TagHandler {

    protected CompositeEmitter body;
    protected Map<String, CompositeEmitter> blocks = null;
    protected Map<String, Expression> attributes = null;

    public void setBody(CompositeEmitter body) {
        this.body = body;
    }

    public void addBlock(String name, CompositeEmitter body) {
        if (blocks == null) {
            blocks = new HashMap<>();
        }
        //TODO warn for duplicate names
        blocks.put(name, body);
    }

    public CompositeEmitter getBlock(String name) {
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

    public CompositeEmitter getBody() {
        return body;
    }

    public abstract void apply(TagHandler parentHandler, CompositeEmitter block);

    public void setAttribute(String name, Expression expression) {
        if (attributes == null) {
            attributes = new HashMap<>();
        }
        //TODO warn for duplicate names
        attributes.put(name, expression);
    }
}
