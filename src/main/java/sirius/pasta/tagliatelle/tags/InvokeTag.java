/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.tagliatelle.tags;

import sirius.kernel.commons.Strings;
import sirius.kernel.di.std.Register;
import sirius.pasta.noodle.Callable;
import sirius.pasta.noodle.ConstantCall;
import sirius.pasta.noodle.compiler.CompileException;
import sirius.pasta.tagliatelle.Template;
import sirius.pasta.tagliatelle.TemplateArgument;
import sirius.pasta.tagliatelle.emitter.CompositeEmitter;

import javax.annotation.Nonnull;
import java.util.Collections;
import java.util.List;

/**
 * Handles <tt>i:invoke</tt> which invokes or inlines a given template.
 */
public class InvokeTag extends TagHandler {

    private static final String ATTR_TEMPLATE = "template";

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
            return Collections.singletonList(new TemplateArgument(String.class,
                                                                  ATTR_TEMPLATE,
                                                                  "Contains the path of the template to render."));
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
            invokeStaticTemplate(template, targetBlock);
        } else if (getAttribute(ATTR_TEMPLATE) != null) {
            invokeDynamicTemplate(getAttribute(ATTR_TEMPLATE), targetBlock);
        }
    }

    protected void invokeStaticTemplate(Template template, CompositeEmitter targetBlock) {
        targetBlock.addChild(getCompilationContext().invokeTemplate(getStartOfTag(),
                                                                    template,
                                                                    this::getAttribute,
                                                                    blocks));
    }

    protected void invokeDynamicTemplate(Callable templateNameSupplier, CompositeEmitter targetBlock) {
        targetBlock.addChild(getCompilationContext().invokeTemplate(getStartOfTag(),
                                                                    templateNameSupplier,
                                                                    this::getAttribute,
                                                                    blocks));
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

        if (ensureThatTemplateIsPresent()) {
            for (TemplateArgument arg : template.getArguments()) {
                if (Strings.areEqual(arg.getName(), name)) {
                    return arg.getType();
                }
            }
            return super.getExpectedAttributeType(name);
        } else {
            // Accept anything, we don't want to report errors based on previous errors...
            return Callable.class;
        }
    }

    private boolean ensureThatTemplateIsPresent() {
        // If the "template" attribute isn't a constant, we can not load the template right now; we will just accept
        // any attribute in `getExpectedAttributeType(String)` and hope for the best...
        if (getAttribute(ATTR_TEMPLATE) != null && !(getAttribute(ATTR_TEMPLATE) instanceof ConstantCall)) {
            templateResolved = true;
            return false;
        }

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
            return template != null;
        }

        return true;
    }
}
