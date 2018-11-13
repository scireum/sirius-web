/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.data;

import sirius.kernel.health.Exceptions;

import java.io.InputStream;
import java.util.function.Predicate;

/**
 * Processes line based input files like MS Excel or CSV.
 */
public interface LineBasedProcessor {

    /**
     * Generates an appropriate LineBasedProcessor based on the file extension of the given file.
     *
     * @param name  the name of the file to process
     * @param input an input stream containing the data to import
     * @return an appropriate processor for the given file
     * @throws sirius.kernel.health.HandledException if no processor can handle the given file
     */
    static LineBasedProcessor create(String name, InputStream input) {
        if (name.toLowerCase().endsWith("xls")) {
            return new XLSProcessor(input, false);
        }
        if (name.toLowerCase().endsWith("xlsx")) {
            return new XLSProcessor(input, true);
        }
        if (name.toLowerCase().endsWith("csv")) {
            return new CSVProcessor(input);
        }
        throw Exceptions.createHandled().withSystemErrorMessage("Cannot process files of type: %s", name).handle();
    }

    /**
     * Starts processing and sends each line to the given rowProcessor.
     *
     * @param rowProcessor the processor which handles each row of the file
     * @param errorHandler errorHandler which gets called on exceptions. returns <tt>true</tt> if exception was handled
     *                     and <tt>false</tt> if exception should be rethrown.
     * @throws Exception in case an error occurred while processing.
     */
    void run(RowProcessor rowProcessor, Predicate<Exception> errorHandler) throws Exception;
}
