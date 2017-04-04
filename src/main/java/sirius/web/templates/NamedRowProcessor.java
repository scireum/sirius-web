/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates;

import sirius.kernel.commons.Value;

import java.util.Map;

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
     * Note that row does not necessarily contain a value for every key, therefore use
     * {@code row.getOrDefault(key, Value.EMPTY)} to safely obtain a non-null value.
     *
     * @param lineNumber the line number which is currently being processed.
     * @param row        the data of the row / line to process
     */
    void handleRow(int lineNumber, Map<String, Value> row);
}
