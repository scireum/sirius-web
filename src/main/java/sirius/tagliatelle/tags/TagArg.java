/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.tags;

import sirius.kernel.di.std.Register;
import sirius.kernel.health.Exceptions;
import sirius.tagliatelle.TagContext;
import sirius.tagliatelle.TemplateArgument;
import sirius.tagliatelle.expression.ConstantNull;
import sirius.tagliatelle.expression.ConstantString;
import sirius.tagliatelle.expression.Expression;

import javax.annotation.Nonnull;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Created by aha on 12.05.17.
 */
public class TagArg extends TagHandler {

    @Register
    public static class Factory implements TagHandlerFactory {

        @Nonnull
        @Override
        public String getName() {
            return "i:arg";
        }

        @Override
        public TagHandler createHandler() {
            return new TagArg();
        }
    }

    private static final Map<String, Class<?>> builtInClasses;

    static {
        builtInClasses = new HashMap<>();
        builtInClasses.put("String", String.class);
        builtInClasses.put("int", int.class);
        builtInClasses.put("boolean", boolean.class);
        builtInClasses.put("List", List.class);
    }

    @Override
    public void apply(TagContext context) {
        String name = getConstantAttribute("name").asString();
        String typeName = getConstantAttribute("type").asString();
        try {
            Class<?> type = builtInClasses.get(typeName);
            if (type == null) {
                type = Class.forName(typeName);
            }
            Expression defaultValue = getAttribute("default");
            if (ConstantNull.NULL.equals(defaultValue)) {
                defaultValue = ConstantString.EMPTY_STRING;
            } else if (defaultValue == null) {
                defaultValue = null;
            }

            if (defaultValue != null && !type.isAssignableFrom(defaultValue.getType())) {
                context.getContext()
                       .error(context.getStartOfTag(),
                              "The default expression for '%s' ('%s') does not match its declared type %s",
                              name,
                              defaultValue,
                              typeName);
            }

            context.getContext().push(name, type);
            context.getContext().getTemplate().addArgument(new TemplateArgument(type, name, defaultValue));
        } catch (ClassNotFoundException e) {
            Exceptions.ignore(e);
            context.getContext().error(context.getStartOfTag(), "Unknown argument type: %s for '%s'", typeName, name);
        }
    }

    @Override
    public Class<?> getExpectedAttributeType(String name) {
        if ("name".equals(name)) {
            return String.class;
        }

        if ("type".equals(name)) {
            return String.class;
        }

        if ("default".equals(name)) {
            return Expression.class;
        }

        return super.getExpectedAttributeType(name);
    }
}
