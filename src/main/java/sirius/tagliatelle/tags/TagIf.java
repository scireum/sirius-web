/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.tags;

import sirius.tagliatelle.emitter.ConditionalEmitter;

/**
 * Created by aha on 12.05.17.
 */
public class TagIf extends TagHandler {
    @Override
    public void apply(TagContext context) {
        ConditionalEmitter result = new ConditionalEmitter(context.getStartOfTag());
        result.setConditionExpression(getAttribute("test"));
        result.setWhenTrue(getBlock("body"));
        result.setWhenFalse(getBlock("else"));
        context.getBlock().addChild(result);
    }

    @Override
    public Class<?> getExpectedAttributeType(String name) {
        if ("test".equals(name)) {
            return boolean.class;
        }

        return super.getExpectedAttributeType(name);
    }
}
