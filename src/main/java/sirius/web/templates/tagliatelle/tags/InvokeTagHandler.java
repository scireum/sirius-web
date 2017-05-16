/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates.tagliatelle.tags;

import sirius.kernel.commons.Strings;
import sirius.web.templates.tagliatelle.TagContext;
import sirius.web.templates.tagliatelle.Template;
import sirius.web.templates.tagliatelle.TemplateArgument;
import sirius.web.templates.tagliatelle.emitter.InvokeTemplateEmitter;
import sirius.web.templates.tagliatelle.expression.Expression;

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
        InvokeTemplateEmitter emitter = new InvokeTemplateEmitter(template.getName());
        if (template.getNumberOfArguments() > 0) {
            Expression[] args = new Expression[template.getNumberOfArguments()];
            int index = 0;
            for (TemplateArgument arg : template.getArguments()) {
                args[index] = getAttribute(arg.getName());
                index++;
            }
            emitter.setArguments(args);
        }
        emitter.setBlocks(blocks);
        context.getBlock().addChild(emitter);
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
