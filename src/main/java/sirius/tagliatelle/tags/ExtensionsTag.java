/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.tags;

import sirius.kernel.commons.Strings;
import sirius.kernel.di.std.Part;
import sirius.kernel.di.std.Register;
import sirius.tagliatelle.Tagliatelle;
import sirius.tagliatelle.TemplateArgument;
import sirius.tagliatelle.TemplateExtension;
import sirius.tagliatelle.emitter.CompositeEmitter;
import sirius.tagliatelle.expression.Expression;

import javax.annotation.Nonnull;
import java.util.Collections;
import java.util.List;

/**
 * Handles <tt>i:extensions</tt> which invokes all extensions with the given name.
 */
public class ExtensionsTag extends InvokeTag {

    private static final String ATTR_TARGET = "target";
    private static final String ATTR_POINT = "point";

    /**
     * Creates new tags of the given type (name).
     */
    @Register
    public static class Factory implements TagHandlerFactory {

        @Nonnull
        @Override
        public String getName() {
            return "i:extensions";
        }

        @Override
        public TagHandler createHandler() {
            return new ExtensionsTag();
        }

        @Override
        public List<TemplateArgument> reportArguments() {
            return Collections.singletonList(new TemplateArgument(String.class,
                                                                  ATTR_POINT,
                                                                  "Contains the name used to fetch all known extensions."));
        }

        @Override
        public String getDescription() {
            return "Invokes all templates which are provided for a given extension point.";
        }
    }

    @Part
    private static Tagliatelle engine;

    @Override
    public void apply(CompositeEmitter targetBlock) {
        String target = getConstantAttribute(ATTR_TARGET).asString();
        if (Strings.isFilled(target)) {
            for (TemplateExtension extension : engine.getExtensions(target)) {
                targetBlock.addChild(getCompilationContext().invokeTemplate(getStartOfTag(),
                                                                            extension.getTemplate(),
                                                                            this::getAttribute,
                                                                            blocks));
            }
        }
    }

    @Override
    public Class<?> getExpectedAttributeType(String name) {
        if (ATTR_TARGET.equals(name)) {
            return String.class;
        }
        if (ATTR_POINT.equals(name)) {
            return String.class;
        }

        return Expression.class;
    }
}
