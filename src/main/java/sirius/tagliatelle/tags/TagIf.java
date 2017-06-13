/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.tags;

import sirius.tagliatelle.emitter.CompositeEmitter;
import sirius.tagliatelle.emitter.ConditionalEmitter;

/**
 * Handles <tt>i:if</tt> which emits its body if a condition is met.
 */
public class TagIf extends TagHandler {
    @Override
    public void apply(CompositeEmitter targetBlock) {
        ConditionalEmitter result = new ConditionalEmitter(getStartOfTag());
        result.setConditionExpression(getAttribute("test"));
        result.setWhenTrue(getBlock("body"));
        result.setWhenFalse(getBlock("else"));
        targetBlock.addChild(result);
    }

    @Override
    public Class<?> getExpectedAttributeType(String name) {
        if ("test".equals(name)) {
            return boolean.class;
        }

        return super.getExpectedAttributeType(name);
    }
}
