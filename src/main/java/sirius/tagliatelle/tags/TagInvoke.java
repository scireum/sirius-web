/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.tags;

import sirius.kernel.di.std.Register;
import sirius.tagliatelle.Template;
import sirius.tagliatelle.compiler.CompileException;
import sirius.tagliatelle.expression.Expression;

import javax.annotation.Nonnull;

/**
 * Created by aha on 12.05.17.
 */
public class TagInvoke extends TagHandler {

    public static final String ATTR_TEMPLATE = "template";
    public static final String ATTR_INLINE = "inline";

    @Register
    public static class Factory implements TagHandlerFactory {

        @Nonnull
        @Override
        public String getName() {
            return "i:invoke";
        }

        @Override
        public TagHandler createHandler() {
            return new TagInvoke();
        }
    }

    @Override
    public void apply(TagContext context) {
        Template template = resolveTemplate(context, getConstantAttribute(ATTR_TEMPLATE).asString());
        if (template != null) {
            invokeTemplate(context, template);
        }
    }

    protected void invokeTemplate(TagContext context, Template template) {
        if (template.getPragma(ATTR_INLINE).asBoolean() || getConstantAttribute(ATTR_INLINE).asBoolean()) {
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

    protected Template resolveTemplate(TagContext context, String templateName) {
        try {
            Template template =
                    context.getContext().resolveTemplate(context.getStartOfTag(), templateName).orElse(null);

            if (template == null) {
                context.getContext()
                       .error(context.getStartOfTag(), "Cannot find the referenced template: %s", templateName);
            }

            return template;
        } catch (CompileException e) {
            context.getContext()
                   .error(context.getStartOfTag(),
                          "Error compiling referenced template: %s%n%s",
                          templateName,
                          e.getMessage());

            return null;
        }
    }

    @Override
    public Class<?> getExpectedAttributeType(String name) {
        if (ATTR_TEMPLATE.equals(name)) {
            return String.class;
        }
        if (ATTR_INLINE.equals(name)) {
            return boolean.class;
        }

        return Expression.class;
    }
}
