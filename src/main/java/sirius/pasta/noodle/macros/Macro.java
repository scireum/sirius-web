/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.macros;

import parsii.tokenizer.Position;
import sirius.kernel.di.std.AutoRegister;
import sirius.kernel.di.std.Named;
import sirius.pasta.noodle.Environment;
import sirius.pasta.noodle.compiler.CompilationContext;
import sirius.pasta.noodle.compiler.ir.Node;

import java.util.List;

/**
 * Represents a macro or constant function.
 * <p>
 * Note that {@link BasicMacro} is a base class which simplifies writing macros. Also note that if a macro is safe
 * to be called from custom code (provided by users of the system), {@link sirius.pasta.noodle.sandbox.PublicAPI}
 * should be added as annotation to the class.
 */
@AutoRegister
public interface Macro extends Named {

    /**
     * Returns the type of objects returned by this macro.
     *
     * @param args the arguments being passed into the macro
     * @return the type of objects returned by this macro
     */
    Class<?> getType(List<Node> args);

    /**
     * Verifies the arguments.
     * <p>
     * This is used to verify the type and count of arguments at compile time. If invalid parameters are detected,
     * either an {@link IllegalArgumentException} can be thrown to report a generic error or the given context can
     * be used to either emit an error or warning.
     *
     * @param context  the compilation context which can be used to emit an error or warning
     * @param position the position to use for the generated errors or warnings
     * @param args     the expressions which will be passed in at runtime.
     */
    void verify(CompilationContext context, Position position, List<Node> args);

    /**
     * Determines if the macro is "constant" for the given parameter expressions.
     *
     * @param context the compilation context which can be used to emit an error or warning
     * @param args    the arguments of the macro
     * @return <tt>true</tt> if the function will always yield the same output for the same input, <tt>false</tt>
     * otherwise.
     */
    boolean isConstant(CompilationContext context, List<Node> args);

    /**
     * Evaluates the macro at runtime.
     *
     * @param environment the execution environment
     * @param args        the arguments
     * @return the result of the macro call
     */
    Object invoke(Environment environment, Object[] args);

    /**
     * Provides a short description of what the macro does for documentation purposes.
     *
     * @return a short description of the macro
     */
    String getDescription();
}
