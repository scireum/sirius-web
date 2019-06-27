/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.macros;

import sirius.kernel.di.std.Named;
import sirius.tagliatelle.expression.Expression;
import sirius.tagliatelle.rendering.LocalRenderContext;

import java.util.List;

/**
 * Represents a macro or constant function.
 */
public interface Macro extends Named {

    /**
     * Returns the type of objects returned by this macro.
     *
     * @return the type of objects returned by this macro
     */
    Class<?> getType();

    /**
     * Verifies the arguments.
     * <p>
     * This is used to verify the type and count of arguments at compile time. If one or more arguments are invalid an
     * {@link IllegalArgumentException} can be thrown.
     *
     * @param args the expressions which will be passed in at runtime.
     */
    void verifyArguments(List<Expression> args);

    /**
     * Evaluates the macro at runtime.
     * <p>
     * Note that the arguments are still expressions and have to be evaluated on demand.
     *
     * @param ctx  the rendering context
     * @param args the arguments
     * @return the result of the macro call
     */
    Object eval(LocalRenderContext ctx, Expression[] args);

    /**
     * Determines if the macro is "constant" for the given parameter expressions.
     *
     * @param args the arguments of the macro
     * @return <tt>true</tt> if the function will always yield the same output for the same input, <tt>false</tt>
     * otherwise.
     */
    boolean isConstant(Expression[] args);

    /**
     * Provides a short description of what the macro does for documentation purposes.
     *
     * @return a short description of the macro
     */
    String getDescription();
}
