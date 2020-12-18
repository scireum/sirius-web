/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.tagliatelle.tags;

import sirius.kernel.di.std.Register;
import sirius.pasta.noodle.Callable;
import sirius.pasta.noodle.compiler.VariableScoper;
import sirius.pasta.tagliatelle.TemplateArgument;
import sirius.pasta.tagliatelle.emitter.CompositeEmitter;
import sirius.pasta.tagliatelle.emitter.PushLocalEmitter;

import javax.annotation.Nonnull;
import java.util.Arrays;
import java.util.List;

/**
 * Handles <tt>i:local</tt> which defines a local variable.
 */
public class LocalTag extends TagHandler {

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
            return "i:local";
        }

        @Override
        public TagHandler createHandler() {
            return new LocalTag();
        }

        @Override
        public List<TemplateArgument> reportArguments() {
            return Arrays.asList(new TemplateArgument(String.class,
                                                      PARAM_NAME,
                                                      "Contains the name of the local variable."),
                                 new TemplateArgument(Callable.class,
                                                      PARAM_VALUE,
                                                      "Contains a expression which is evaluated into the local variable."));
        }

        @Override
        public String getDescription() {
            return "Defines a local variable to access the invocation result of an expression without re-evaluating it.";
        }
    }

    @Override
    public void apply(CompositeEmitter targetBlock) {
        String name = getConstantAttribute(PARAM_NAME).asString();
        Callable value = getAttribute(PARAM_VALUE);
        if (value != null) {
            VariableScoper.Variable variable = getCompilationContext().getVariableScoper()
                                                                      .defineVariable(getStartOfTag(),
                                                                                      name,
                                                                                      value.getType(),
                                                                                      value.getGenericType());
            targetBlock.addChild(new PushLocalEmitter(startOfTag, variable.getLocalIndex(), value));
        } else {
            compilationContext.error(startOfTag, "The attribute value is required.");
        }
    }

    @Override
    public void afterTag() {
        // Prevent popping the defined variable...
    }

    @Override
    public Class<?> getExpectedAttributeType(String name) {
        if (PARAM_NAME.equals(name)) {
            return String.class;
        }

        if (PARAM_VALUE.equals(name)) {
            return Callable.class;
        }

        return super.getExpectedAttributeType(name);
    }
}
