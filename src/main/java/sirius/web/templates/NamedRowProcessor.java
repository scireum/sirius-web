/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates;

/**
 * Invoked by a {@link SmartLineBasedProcessor} to handle one row.
 */
public interface NamedRowProcessor {
    /**
     * Called the handle a row of an input file.
     * <p>
     * Note that an exception thrown here will completely abort the whole process. Therefore proper exception
     * handling is required.
     * <p>
     *
     * @param lineNumber the line number which is currently being processed.
     * @param row        the data of the row to process
     */
    void handleRow(int lineNumber, SmartRow row);
}
