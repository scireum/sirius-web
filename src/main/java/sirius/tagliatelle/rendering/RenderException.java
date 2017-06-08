/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.rendering;

import sirius.kernel.commons.Strings;
import sirius.kernel.health.HandledException;
import sirius.tagliatelle.compiler.CompileException;

import java.io.FileNotFoundException;

/**
 * Thrown for any error or exception when rendering a template.
 * <p>
 * This contains the render stack (a stacktrace for templates) next to the original error and exception type as message.
 */
public class RenderException extends Exception {

    private static final long serialVersionUID = -3342628287682148L;

    private RenderException(String message, Exception ex) {
        super(message, ex);
    }

    /**
     * Creates a new error for the given contexnt and root exception.
     *
     * @param context the context used to determine the render stack
     * @param ex      the root exception
     * @return a RenderException with an appropriate error messsage
     */
    public static RenderException create(LocalRenderContext context, Exception ex) {
        if (ex instanceof RenderException) {
            return (RenderException) ex;
        }

        String renderStack = Strings.apply("%nRender Stack:%n------------%n%s%n", ex.getMessage(), context.toString());
        if (ex instanceof CompileException || ex instanceof HandledException) {
            return new RenderException(Strings.apply("%s%s", ex.getMessage(), renderStack), ex);
        }

        if (ex instanceof FileNotFoundException) {
            return new RenderException(Strings.apply("Cannot find the template: %s%s", ex.getMessage(), renderStack),
                                       ex);
        }

        return new RenderException(Strings.apply("A runtime error occurred: %s (%s)%s",
                                                 ex.getMessage(),
                                                 ex.getClass().getName(),
                                                 renderStack), ex);
    }
}
