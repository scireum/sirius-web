/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.data;

import sirius.kernel.async.TaskContext;
import sirius.kernel.commons.BOMReader;
import sirius.kernel.commons.CSVReader;
import sirius.kernel.commons.RateLimit;
import sirius.kernel.nls.NLS;

import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Predicate;

/**
 * In charge of processing CSV (comma separated values) files.
 */
public class CSVProcessor implements LineBasedProcessor {

    private InputStream input;

    CSVProcessor(InputStream input) {
        super();
        this.input = input;
    }

    @Override
    public void run(RowProcessor rowProcessor, Predicate<Exception> errorHandler) throws Exception {
        CSVReader reader = new CSVReader(new BOMReader(new InputStreamReader(input, StandardCharsets.UTF_8)));
        AtomicInteger rowCounter = new AtomicInteger(0);
        TaskContext tc = TaskContext.get();

        reader.execute(row -> {
            try {
                rowProcessor.handleRow(rowCounter.incrementAndGet(), row);
                    tc.tryUpdateState(NLS.get("LineBasedProcessor.linesProcessed"), rowCounter.get());
            } catch (Exception e) {
                if (!errorHandler.test(e)) {
                    throw e;
                }
            }
        });

        if (tc.isActive() && rowCounter.get() > 0) {
            tc.forceUpdateState(NLS.get("LineBasedProcessor.linesProcessed"), rowCounter.get());
        }
    }
}
