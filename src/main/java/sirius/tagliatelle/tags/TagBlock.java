/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.tags;

import sirius.kernel.commons.Strings;
import sirius.tagliatelle.TagContext;

/**
 * Created by aha on 12.05.17.
 */
public class TagBlock extends TagHandler {

    @Override
    public void apply(TagContext context) {
        if (context.getParentHandler() != null) {

            String name = getConstantAttribute("name").asString();
            if (Strings.isEmpty(name)) {
                context.getContext()
                       .warning(context.getStartOfTag(), "The attribute name if i:block must be filled.", name);
            } else {
                if (context.getParentHandler().getBlock(name) != null) {
                    context.getContext()
                           .warning(context.getStartOfTag(),
                                    "Duplicate i:block. A block for '%s' is already present.",
                                    name);
                }

                context.getParentHandler().addBlock(name, getBlock("body"));
            }
        } else {
            context.getContext().warning(context.getStartOfTag(), "Cannot define a block without a surrounding tag.");
        }
    }

    @Override
    public Class<?> getExpectedAttributeType(String name) {
        if ("name".equals(name)) {
            return String.class;
        }

        return super.getExpectedAttributeType(name);
    }
}
