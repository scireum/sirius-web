/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle;

import java.io.Serial;

/**
 * Represents an exception which happens during the execution of the runtime.
 * <p>
 * Please note that this is only used when an exception within scripting code occurs. If the exception is already
 * a {@link sirius.kernel.health.HandledException}, it is re-thrown as such. Also if a hard VM/interpreter error
 * occurs (i.e. a stack underflow), also a <tt>HandledException</tt> is thrown rather than this exception.
 * <p>
 * So this exception is only used for errors "in the script". This way, the calling code can decide if this
 * should be handled and logged to the system or, in case of user provided script, simply reported to the caller
 * but not logged to the system logs.
 */
public class ScriptingException extends Exception {

    @Serial
    private static final long serialVersionUID = -1743664824689650688L;

    /**
     * Creates a new exception with the given message.
     *
     * @param message the error to report
     */
    public ScriptingException(String message) {
        super(message);
    }

    /**
     * Creates a new exception with the given message and cause.
     *
     * @param message the error to report
     * @param cause   the root cause of this exception
     */
    public ScriptingException(String message, Throwable cause) {
        super(message, cause);
    }
}
