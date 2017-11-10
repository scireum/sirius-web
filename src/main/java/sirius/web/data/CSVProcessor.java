/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.data;

import com.google.common.base.Charsets;
import sirius.kernel.async.TaskContext;
import sirius.kernel.commons.BOMReader;
import sirius.kernel.commons.CSVReader;
import sirius.kernel.nls.NLS;

import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.concurrent.atomic.AtomicInteger;

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
    public void run(RowProcessor rowProcessor) throws Exception {
        CSVReader reader = new CSVReader(new BOMReader(new InputStreamReader(input, Charsets.UTF_8)));
        AtomicInteger rowCounter = new AtomicInteger(0);
        TaskContext tc = TaskContext.get();

        reader.execute(row -> {
            rowProcessor.handleRow(rowCounter.incrementAndGet(), row);
            tc.setState(NLS.get("LineBasedProcessor.linesProcessed"), rowCounter.get());
        });
    }
}
