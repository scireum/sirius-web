/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates.engine.expression;

import sirius.kernel.health.Exceptions;
import sirius.web.templates.engine.RenderContext;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

/**
 * Created by aha on 10.05.17.
 */
public class StaticMethodCall extends Expression {

    private Method method;
    private Expression[] parameters;

    @Override
    public Object eval(RenderContext ctx) {
        try {
            if (parameters == null) {
                return method.invoke(null);
            }
            Object[] params = new Object[parameters.length];
            for(int i = 0; i < parameters.length; i++) {
                params[i] = parameters[i].eval(ctx);
            }

            return method.invoke(null, params);
        } catch (IllegalAccessException | InvocationTargetException e) {
            //TODO
            throw Exceptions.handle(e);
        }
    }

    @Override
    public Class<?> getType() {
        return method.getReturnType();
    }
}
