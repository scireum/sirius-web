/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.data;

import sirius.kernel.commons.Explain;
import sirius.kernel.commons.Files;
import sirius.kernel.health.Exceptions;

import java.io.InputStream;
import java.util.function.Predicate;

/**
 * Processes line based input files like MS Excel or CSV.
 */
@SuppressWarnings("java:S1610")
@Explain("We dont use an interface with static methods here as it results in test failures because of reflections")
public abstract class LineBasedProcessor {

    /**
     * Generates an appropriate LineBasedProcessor based on the file extension of the given file.
     *
     * @param name            the name of the file to process
     * @param input           an input stream containing the data to import
     * @param importAllSheets determines if all sheets or just the first should be processed
     * @return an appropriate processor for the given file
     * @throws sirius.kernel.health.HandledException if no processor can handle the given file
     */
    public static LineBasedProcessor create(String name, InputStream input, boolean importAllSheets) {
        if (name.toLowerCase().endsWith("xls")) {
            return new XLSProcessor(input, importAllSheets);
        }
        if (name.toLowerCase().endsWith("xlsx")) {
            return new XLSXProcessor(input, importAllSheets);
        }
        if (name.toLowerCase().endsWith("csv")) {
            return new CSVProcessor(input);
        }
        throw Exceptions.createHandled()
                        .withNLSKey("LineBasedProcessor.error.invalidFileType")
                        .set("type", Files.getFileExtension(name))
                        .handle();
    }

    /**
     * Generates an appropriate LineBasedProcessor based on the file extension of the given file.
     *
     * @param name  the name of the file to process
     * @param input an input stream containing the data to import
     * @return an appropriate processor for the given file
     * @throws sirius.kernel.health.HandledException if no processor can handle the given file
     */
    public static LineBasedProcessor create(String name, InputStream input) {
        return create(name, input, false);
    }

    /**
     * Starts processing and sends each line to the given rowProcessor.
     *
     * @param rowProcessor the processor which handles each row of the file
     * @param errorHandler errorHandler which gets called on exceptions. returns <tt>true</tt> if the exception was
     *                     handled and <tt>false</tt> if the exception should be rethrown.
     * @throws Exception in case an error occurred while processing.
     */
    public abstract void run(RowProcessor rowProcessor, Predicate<Exception> errorHandler) throws Exception;
}
