/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.data;

import java.util.function.Predicate;

/**
 * Provides the necessary information for {@link XLSProcessor} to handle an XLS (MS Excel) sheet.
 */
public class SheetBasedRowProcessor {

    private final RowProcessor rowProcessor;
    private final Predicate<Exception> errorHandler;
    private final String sheetName;

    /**
     * Creates a processor for an XLS (MS Excel) sheet with the given configuration.
     *
     * @param rowProcessor the processor which handles each row of the file
     * @param errorHandler errorHandler which gets called on exceptions. returns <tt>true</tt> if the exception was
     *                     handled and <tt>false</tt> if the exception should be rethrown.
     * @param sheetName    the name of the sheet to process
     */
    public SheetBasedRowProcessor(RowProcessor rowProcessor, Predicate<Exception> errorHandler, String sheetName) {
        this.rowProcessor = rowProcessor;
        this.errorHandler = errorHandler;
        this.sheetName = sheetName;
    }

    public RowProcessor getRowProcessor() {
        return rowProcessor;
    }

    public Predicate<Exception> getErrorHandler() {
        return errorHandler;
    }

    public String getSheetName() {
        return sheetName;
    }
}
