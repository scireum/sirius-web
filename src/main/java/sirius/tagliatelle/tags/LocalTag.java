/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.tags;

import sirius.kernel.di.std.Register;
import sirius.tagliatelle.TemplateArgument;
import sirius.tagliatelle.emitter.CompositeEmitter;
import sirius.tagliatelle.emitter.PushLocalEmitter;
import sirius.tagliatelle.expression.Expression;

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
                                                      "Contains the name of the local variable.",
                                                      null),
                                 new TemplateArgument(Expression.class,
                                                      PARAM_VALUE,
                                                      "Contains a expression which is evaluated into the local variable.",
                                                      null));
        }

        @Override
        public String getDescription() {
            return "Defines a local variable to access the invocation result of an expression without re-evaluating it.";
        }
    }

    @Override
    public void apply(CompositeEmitter targetBlock) {
        String name = getConstantAttribute(PARAM_NAME).asString();
        Expression value = getAttribute(PARAM_VALUE);
        if (value != null) {
            int variable = getCompilationContext().push(getStartOfTag(), name, value.getType());
            targetBlock.addChild(new PushLocalEmitter(startOfTag, variable, value));
        } else {
            compilationContext.error(startOfTag, "The attribute value is required.");
        }
    }

    @Override
    public Class<?> getExpectedAttributeType(String name) {
        if (PARAM_NAME.equals(name)) {
            return String.class;
        }

        if (PARAM_VALUE.equals(name)) {
            return Expression.class;
        }

        return super.getExpectedAttributeType(name);
    }
}
