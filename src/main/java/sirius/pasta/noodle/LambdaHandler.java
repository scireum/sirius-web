/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.Arrays;

/**
 * Provides an invocation handler to be used with {@link java.lang.reflect.Proxy} in order to represent a
 * lambda expression within Noodle.
 */
class LambdaHandler implements InvocationHandler {

    private final int initialIP;
    private final int contextOffset;
    private final int numLocals;
    private final InterpreterCall compiledMethod;
    private final Environment closureEnvironment;

    LambdaHandler(int initialIP,
                  int contextOffset,
                  int numLocals,
                  InterpreterCall compiledMethod,
                  Environment closureEnvironment) {
        this.initialIP = initialIP;
        this.contextOffset = contextOffset;
        this.numLocals = numLocals;
        this.compiledMethod = compiledMethod;
        this.closureEnvironment = closureEnvironment;
    }

    @Override
    public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
        try {
            // Delegate all standard methods or default ones back to the class.
            if (!Modifier.isAbstract(method.getModifiers()) || method.isDefault()) {
                return method.invoke(proxy);
            }

            Environment environment = LambdaEnvironment.create(closureEnvironment, contextOffset, numLocals);

            if (args != null) {
                // Transfer arguments...
                for (int i = 0; i < Math.min(args.length, numLocals); i++) {
                    environment.writeVariable(contextOffset + i, args[i]);
                }
            }

            // Creates another invocation, with appropriate instruction pointer offset, custom stack and custom
            // environment...
            Invocation invocation = new Invocation(compiledMethod, environment, initialIP);

            // run the interpreter...
            return invocation.execute();
        } catch (RuntimeException e) {
            throw e;
        } catch (Exception e) {
            if (Arrays.stream(method.getExceptionTypes())
                      .anyMatch(acceptedException -> acceptedException.isAssignableFrom(e.getClass()))) {
                throw e;
            } else {
                throw new ScriptingException("An error occurred within a lambda: "
                                             + e.getMessage()
                                             + " ("
                                             + e.getClass()
                                             + ")", e);
            }
        }
    }
}
