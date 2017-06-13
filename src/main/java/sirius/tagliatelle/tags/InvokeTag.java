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
import sirius.tagliatelle.emitter.CompositeEmitter;
import sirius.tagliatelle.expression.Expression;

import javax.annotation.Nonnull;

/**
 * Handles <tt>i:invoke</tt> which invokes or inlines a given template.
 */
public class InvokeTag extends TagHandler {

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
            return new InvokeTag();
        }
    }

    @Override
    public void apply(CompositeEmitter targetBlock) {
        Template template = resolveTemplate(getConstantAttribute(ATTR_TEMPLATE).asString());
        if (template != null) {
            invokeTemplate(template, targetBlock);
        }
    }

    protected void invokeTemplate(Template template, CompositeEmitter targetBlock) {
        if (template.getPragma(ATTR_INLINE).asBoolean() || getConstantAttribute(ATTR_INLINE).asBoolean()) {
            targetBlock.addChild(getCompilationContext().inlineTemplate(getStartOfTag(),
                                                                        template,
                                                                        this::getAttribute,
                                                                        this::getBlock));
        } else {
            targetBlock.addChild(getCompilationContext().invokeTemplate(getStartOfTag(),
                                                                        template,
                                                                        this::getAttribute,
                                                                        blocks));
        }
    }

    protected Template resolveTemplate(String templateName) {
        try {
            Template template = getCompilationContext().resolveTemplate(getStartOfTag(), templateName).orElse(null);

            if (template == null) {
                getCompilationContext().error(getStartOfTag(), "Cannot find the referenced template: %s", templateName);
            }

            return template;
        } catch (CompileException e) {
            getCompilationContext().error(getStartOfTag(),
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
