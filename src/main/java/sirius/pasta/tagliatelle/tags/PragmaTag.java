/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.tagliatelle.tags;

import sirius.kernel.commons.Value;
import sirius.kernel.di.std.Register;
import sirius.pasta.tagliatelle.TemplateArgument;
import sirius.pasta.tagliatelle.emitter.CompositeEmitter;

import javax.annotation.Nonnull;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * Handles <tt>i:pragma</tt> which defines a pragma (key / value pair) for a template.
 */
public class PragmaTag extends TagHandler {

    protected static final String PARAM_NAME = "name";
    protected static final String PARAM_VALUE = "value";

    /**
     * Creates new tags of the given type (name).
     */
    @Register
    public static class Factory implements TagHandlerFactory {

        @Nonnull
        @Override
        public String getName() {
            return "i:pragma";
        }

        @Override
        public TagHandler createHandler() {
            return new PragmaTag();
        }

        @Override
        public List<TemplateArgument> reportArguments() {
            return Arrays.asList(new TemplateArgument(String.class, PARAM_NAME, "Contains the name of the pragma"),
                                 new TemplateArgument(String.class, PARAM_VALUE, "Contains the values of the pragma"));
        }

        @Override
        public String getDescription() {
            return "Defines additional infos used by the compiler or the Tagliatelle engine.";
        }
    }

    @Override
    public void apply(CompositeEmitter targetBlock) {
        Value value = getConstantAttribute(PARAM_VALUE);
        if (value.isFilled()) {
            getCompilationContext().getTemplate().addPragma(getConstantAttribute(PARAM_NAME).asString(), value.asString());
        } else if (getBlock("body") != null) {
            getCompilationContext().getTemplate()
                                   .addPragma(getConstantAttribute(PARAM_NAME).asString(), getBlock("body").toString());
        } else {
            compilationContext.error(getStartOfTag(), "A pragma tag must either have a value or a body!");
        }
    }

    @Override
    public Class<?> getExpectedAttributeType(String name) {
        if (PARAM_NAME.equals(name)) {
            return String.class;
        }

        if (PARAM_VALUE.equals(name)) {
            return String.class;
        }

        return super.getExpectedAttributeType(name);
    }

    @Override
    public Set<String> getRequiredAttributeNames() {
        return Set.of(PARAM_NAME);
    }
}
