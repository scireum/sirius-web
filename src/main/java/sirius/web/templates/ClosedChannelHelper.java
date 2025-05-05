/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates;

import com.lowagie.text.ExceptionConverter;
import org.asynchttpclient.exception.ChannelClosedException;

import java.nio.channels.ClosedChannelException;

/**
 * Helps to detect closed channel exceptions in complex exception chains.
 */
public class ClosedChannelHelper {

    private ClosedChannelHelper() {
        // Prevent instantiation
    }

    /**
     * Detects if the given exception or any of its causes is a closed channel exception.
     * <p>
     * This is useful for handling cases where a channel was closed unexpectedly, such as when a client
     * disconnects or a network error occurs.
     * <p>
     * It also handles the async http client variant and the openpdf exception wrapping.
     *
     * @param throwable the exception to check
     * @return <tt>true</tt> if the exception or any of its causes is a closed channel exception.
     */
    public static boolean isCausedByClosedChannel(Throwable throwable) {
        int depth = 0;
        while (throwable != null && depth < 1000) {
            if (throwable instanceof ClosedChannelException || throwable instanceof ChannelClosedException) {
                return true;
            }
            if (throwable instanceof ExceptionConverter exceptionConverter) {
                throwable = exceptionConverter.getException();
            } else {
                throwable = throwable.getCause();
            }
            depth++;
        }
        return false;
    }
}
