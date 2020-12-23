/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.tagliatelle.tags;

import sirius.kernel.di.std.Register;
import sirius.kernel.health.Exceptions;
import sirius.pasta.noodle.Callable;
import sirius.pasta.noodle.ConstantCall;
import sirius.pasta.noodle.compiler.CompilationContext;
import sirius.pasta.tagliatelle.TemplateArgument;
import sirius.pasta.tagliatelle.emitter.CompositeEmitter;

import javax.annotation.Nonnull;
import java.util.Arrays;
import java.util.List;

/**
 * Handles <tt>i:arg</tt> which specifies a template argument.
 */
public class ArgTag extends TagHandler {

    private static final String PARAM_NAME = "name";
    private static final String PARAM_DESCRIPTION = "description";
    private static final String PARAM_DEPRECATED = "deprecated";
    private static final String PARAM_TYPE = "type";
    private static final String PARAM_DEFAULT = "default";

    /**
     * Creates new tags of the given type (name).
     */
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
                                                      "Contains the type name of the argument."),
                                 new TemplateArgument(String.class, PARAM_NAME, "Contains the name of the argument."),
                                 new TemplateArgument(String.class,
                                                      PARAM_DESCRIPTION,
                                                      "Contains a short description of the argument.",
                                                      ConstantCall.EMPTY_STRING,
                                                      null),
                                 new TemplateArgument(Callable.class,
                                                      PARAM_DEFAULT,
                                                      "Contains a default expression which is used,"
                                                      + " if no value is given.",
                                                      ConstantCall.NULL,
                                                      null),
                                 new TemplateArgument(Callable.class,
                                                      PARAM_DEPRECATED,
                                                      "Contains a warning to indicate that the argument"
                                                      + " is deprecated and should no longer be used.",
                                                      ConstantCall.NULL,
                                                      null));
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
        String deprecationWarning = getConstantAttribute(PARAM_DEPRECATED).getString();
        String typeName = getConstantAttribute(PARAM_TYPE).asString();
        Class<?> type = getCompilationContext().resolveClass(getStartOfTag(), typeName);
        Callable defaultValue = getAttribute(PARAM_DEFAULT);
        if (ConstantCall.isConstantNull(defaultValue) && String.class.equals(type)) {
            defaultValue = ConstantCall.EMPTY_STRING;
        }

        if (!isValidDefaultValue(type, defaultValue)) {
            getCompilationContext().error(getStartOfTag(),
                                          "The default expression for '%s' ('%s') does not match its declared type %s",
                                          name,
                                          defaultValue,
                                          typeName);
        }

        getCompilationContext().getVariableScoper().defineVariable(getStartOfTag(), name, type, type);
        getCompilationContext().getTemplate()
                               .addArgument(new TemplateArgument(type,
                                                                 name,
                                                                 description,
                                                                 defaultValue,
                                                                 deprecationWarning));
    }

    @Override
    public void afterTag() {
        // Prevent popping the defined variable...
    }

    private boolean isValidDefaultValue(Class<?> type, Callable defaultValue) {
        if (defaultValue == null) {
            return true;
        }

        return CompilationContext.isAssignableTo(defaultValue.getType(), type);
    }

    @Override
    public Class<?> getExpectedAttributeType(String name) {
        if (PARAM_NAME.equals(name)) {
            return String.class;
        }

        if (PARAM_TYPE.equals(name)) {
            return String.class;
        }

        if (PARAM_DESCRIPTION.equals(name)) {
            return String.class;
        }

        if (PARAM_DEPRECATED.equals(name)) {
            return String.class;
        }

        if (PARAM_DEFAULT.equals(name)) {
            try {
                return getCompilationContext().resolveClass(getStartOfTag(),
                                                            getConstantAttribute(PARAM_TYPE).asString());
            } catch (Exception e) {
                // In case no or an unknown type is given, we simply return Expression.class which suits all arguments.
                // An error has already or will be reported by the framework anyway..
                Exceptions.ignore(e);
                return Callable.class;
            }
        }

        return super.getExpectedAttributeType(name);
    }
}
