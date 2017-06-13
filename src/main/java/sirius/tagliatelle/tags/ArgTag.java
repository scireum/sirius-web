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
import sirius.tagliatelle.expression.ConstantNull;
import sirius.tagliatelle.expression.ConstantString;
import sirius.tagliatelle.expression.Expression;

import javax.annotation.Nonnull;

/**
 * Handles <tt>i:arg</tt> which specifies a template argument.
 */
public class ArgTag extends TagHandler {

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
    }

    @Override
    public void apply(CompositeEmitter targetBlock) {
        String name = getConstantAttribute("name").asString();
        String typeName = getConstantAttribute("type").asString();
        Class<?> type = getCompilationContext().resolveClass(getStartOfTag(), typeName);
        Expression defaultValue = getAttribute("default");
        if (ConstantNull.NULL.equals(defaultValue) && String.class.equals(type)) {
            defaultValue = ConstantString.EMPTY_STRING;
        }

        if (defaultValue != null && !type.isAssignableFrom(defaultValue.getType())) {
            getCompilationContext().error(getStartOfTag(),
                                          "The default expression for '%s' ('%s') does not match its declared type %s",
                                          name,
                                          defaultValue,
                                          typeName);
        }

        getCompilationContext().push(getStartOfTag(), name, type);
        getCompilationContext().getTemplate().addArgument(new TemplateArgument(type, name, defaultValue));
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
