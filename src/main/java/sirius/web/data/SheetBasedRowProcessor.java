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
 *
 * @param rowProcessor the processor which handles each row of the file
 * @param errorHandler errorHandler which gets called on exceptions. returns <tt>true</tt> if the exception was
 *                     handled and <tt>false</tt> if the exception should be rethrown.
 * @param sheetName    the name of the sheet to process
 */
public record SheetBasedRowProcessor(RowProcessor rowProcessor, Predicate<Exception> errorHandler, String sheetName) {
}
