/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.tags;

import sirius.kernel.commons.Strings;
import sirius.tagliatelle.Template;
import sirius.tagliatelle.TemplateArgument;

/**
 * Created by aha on 16.05.17.
 */
public class InvokeTagHandler extends TagHandler {

    private Template template;

    public InvokeTagHandler(Template template) {
        this.template = template;
    }

    @Override
    public void apply(TagContext context) {
        if (template.getPragma(TagInvoke.ATTR_INLINE).asBoolean()) {
            context.getBlock()
                   .addChild(context.getContext()
                                    .inlineTemplate(context.getStartOfTag(),
                                                    template,
                                                    this::getAttribute,
                                                    this::getBlock));
        } else {
            context.getBlock()
                   .addChild(context.getContext()
                                    .invokeTemplate(context.getStartOfTag(), template, this::getAttribute, blocks));
        }
    }

    @Override
    public Class<?> getExpectedAttributeType(String name) {
        for (TemplateArgument arg : template.getArguments()) {
            if (Strings.areEqual(arg.getName(), name)) {
                return arg.getType();
            }
        }

        return super.getExpectedAttributeType(name);
    }
}
