/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.tags;

import sirius.kernel.commons.Strings;
import sirius.kernel.di.std.Register;
import sirius.tagliatelle.Template;
import sirius.tagliatelle.TemplateArgument;
import sirius.tagliatelle.compiler.CompileException;
import sirius.tagliatelle.emitter.CompositeEmitter;
import sirius.tagliatelle.expression.ConstantBoolean;
import sirius.tagliatelle.expression.Expression;

import javax.annotation.Nonnull;
import java.util.Arrays;
import java.util.List;

/**
 * Handles <tt>i:invoke</tt> which invokes or inlines a given template.
 */
public class InvokeTag extends TagHandler {

    private static final String ATTR_TEMPLATE = "template";
    protected static final String ATTR_INLINE = "inline";

    /**
     * Creates new tags of the given type (name).
     */
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

        @Override
        public List<TemplateArgument> reportArguments() {
            return Arrays.asList(new TemplateArgument(String.class,
                                                      ATTR_TEMPLATE,
                                                      "Contains the path of the template to render."),
                                 new TemplateArgument(boolean.class,
                                                      ATTR_INLINE,
                                                      "Determines if the invocation should be actually "
                                                      + "inlined int othe calling template.",
                                                      ConstantBoolean.FALSE,
                                                      null));
        }

        @Override
        public String getDescription() {
            return "Invokes a template. Note that all template arguments also have to be passed as tag attributes.";
        }
    }

    private Template template;
    private boolean templateResolved;

    @Override
    public void apply(CompositeEmitter targetBlock) {
        if (!templateResolved) {
            template = resolveTemplate(getConstantAttribute(ATTR_TEMPLATE).asString());
        }

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
            Template result = getCompilationContext().resolveTemplate(getStartOfTag(), templateName).orElse(null);

            if (result == null) {
                getCompilationContext().error(getStartOfTag(), "Cannot find the referenced template: %s", templateName);
            }

            return result;
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

        if (ensureThatTemplateIsPresent()) {
            for (TemplateArgument arg : template.getArguments()) {
                if (Strings.areEqual(arg.getName(), name)) {
                    return arg.getType();
                }
            }
            return super.getExpectedAttributeType(name);
        } else {
            // Accept anything, we don't want to report errors based on previous errors...
            return Expression.class;
        }
    }

    private boolean ensureThatTemplateIsPresent() {
        if (template == null) {

            if (templateResolved) {
                // We already tried and failed...
                return false;
            }
            templateResolved = true;

            // If the "template" isn't the first attribute, we're screwed...complain to the user....
            if (getConstantAttribute(ATTR_TEMPLATE).isEmptyString()) {
                getCompilationContext().error(getStartOfTag(),
                                              "Please provide the template parameter first, "
                                              + "so that the given attributes can be checked");
                return false;
            }

            // Try to resolve
            template = resolveTemplate(getConstantAttribute(ATTR_TEMPLATE).asString());
            // No template, no parameter checking, just give up, an error has been reported...
            if (template == null) {
                return false;
            }
        }

        return true;
    }
}
