/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.data;

import sirius.kernel.commons.Values;

/**
 * Invoked by a {@link LineBasedProcessor} to handle one row.
 */
public interface RowProcessor {

    /**
     * Called the handle a row of an input file.
     * <p>
     * Note that an exception thrown here will completely abort the whole process if {@link #handleError(Exception)} is
     * not implemented. Therefore proper exception handling is required.
     * </p>
     *
     * @param lineNumber the line number which is currently being processed.
     * @param row        the data of the row / line to process
     */
    void handleRow(int lineNumber, Values row);

    /**
     * Called if an {@link Exception} is thrown while parsing a row.
     *
     * @param error The {@link Exception} which was thrown
     * @return <tt>true</tt> to continue execution, <tt>false</tt> to abort execution
     */
    default boolean handleError(Exception error) {
        return false;
    }
}
