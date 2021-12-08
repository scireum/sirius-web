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
 * Invoked by an {@link XLSProcessor} to handle one row in an Excel sheet.
 */
public interface SheetBasedRowProcessor extends RowProcessor {

    /**
     * Called to handle a row in a sheet of an input file.
     * <p>
     * Note that an exception thrown here will completely abort the whole process. Therefore proper exception
     * handling is required.
     * </p>
     *
     * @param sheetName  the name of the sheet which is currently being processed
     * @param lineNumber the line number which is currently being processed.
     * @param row        the data of the row / line to process
     */
    void handleRow(String sheetName, int lineNumber, Values row);
}
