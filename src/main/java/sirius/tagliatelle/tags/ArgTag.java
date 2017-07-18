/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.tags;

import sirius.kernel.di.std.Register;
import sirius.tagliatelle.Tagliatelle;
import sirius.tagliatelle.TemplateArgument;
import sirius.tagliatelle.emitter.CompositeEmitter;
import sirius.tagliatelle.expression.ConstantNull;
import sirius.tagliatelle.expression.ConstantString;
import sirius.tagliatelle.expression.Expression;

import javax.annotation.Nonnull;
import java.util.Arrays;
import java.util.List;

/**
 * Handles <tt>i:arg</tt> which specifies a template argument.
 */
public class ArgTag extends TagHandler {

    public static final String PARAM_NAME = "name";
    public static final String PARAM_DESCRIPTION = "description";
    public static final String PARAM_TYPE = "type";
    public static final String PARAM_DEFAULT = "default";

    @Register
    public static class Factory implements TagHandlerFactory {

        @Nonnull
        @Override
        public String getName() {
            return "i:arg";
        }

        @Override
        public TagHandler createHandler() {
            return new ArgTag();
        }

        @Override
        public List<TemplateArgument> reportArguments() {
            return Arrays.asList(new TemplateArgument(String.class,
                                                      PARAM_TYPE,
                                                      "Contains the type name of the argument.",
                                                      null),
                                 new TemplateArgument(String.class,
                                                      PARAM_NAME,
                                                      "Contains the name of the argument.",
                                                      null),
                                 new TemplateArgument(String.class,
                                                      PARAM_DESCRIPTION,
                                                      "Contains a short description of the argument.",
                                                      ConstantString.EMPTY_STRING),
                                 new TemplateArgument(Expression.class,
                                                      PARAM_DEFAULT,
                                                      "Contains a default expression which is used, if no value is given.",
                                                      ConstantNull.NULL));
        }

        @Override
        public String getDescription() {
            return "Declares a template argument.";
        }
    }

    @Override
    public void apply(CompositeEmitter targetBlock) {
        String name = getConstantAttribute(PARAM_NAME).asString();
        String description = getConstantAttribute(PARAM_DESCRIPTION).asString();
        String typeName = getConstantAttribute(PARAM_TYPE).asString();
        Class<?> type = getCompilationContext().resolveClass(getStartOfTag(), typeName);
        Expression defaultValue = getAttribute(PARAM_DEFAULT);
        if (ConstantNull.NULL.equals(defaultValue) && String.class.equals(type)) {
            defaultValue = ConstantString.EMPTY_STRING;
        }

        if (!isValidDefaultValue(type, defaultValue)) {
            getCompilationContext().error(getStartOfTag(),
                                          "The default expression for '%s' ('%s') does not match its declared type %s",
                                          name,
                                          defaultValue,
                                          typeName);
        }

        getCompilationContext().push(getStartOfTag(), name, type);
        getCompilationContext().getTemplate().addArgument(new TemplateArgument(type, name, description, defaultValue));
    }

    private boolean isValidDefaultValue(Class<?> type, Expression defaultValue) {
        if (defaultValue == null) {
            return true;
        }

        return Tagliatelle.isAssignableTo(defaultValue.getType(), type);
    }

    @Override
    public Class<?> getExpectedAttributeType(String name) {
        if (PARAM_NAME.equals(name)) {
            return String.class;
        }

        if (PARAM_TYPE.equals(name)) {
            return String.class;
        }

        if (PARAM_DEFAULT.equals(name)) {
            return Expression.class;
        }

        return super.getExpectedAttributeType(name);
    }
}
