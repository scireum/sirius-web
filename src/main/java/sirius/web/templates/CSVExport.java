/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates;

import au.com.bytecode.opencsv.CSVWriter;
import sirius.kernel.nls.NLS;

import java.io.Writer;
import java.util.List;

/**
 * Generates a CSV file as export.
 *
 * @author Andreas Haufler (aha@scireum.de)
 * @since 2015/02
 */
public class CSVExport {

    protected CSVWriter writer;

    /**
     * Generates a new Export
     *
     * @param output the destination for the generated CSV
     */
    public CSVExport(Writer output) {
        writer = new CSVWriter(output, ';', '"');
    }

    /**
     * Adds the given array of objects as a row.
     *
     * @param row the objects to add to the table
     * @return the export itself for fluent method calls
     */
    public CSVExport addRow(Object... row) {
        String[] arr = new String[row.length];
        for (int i = 0; i < arr.length; i++) {
            arr[i] = NLS.toUserString(row[i]);
        }
        writer.writeNext(arr);

        return this;
    }

    /**
     * Adds the given list of objects as a row.
     *
     * @param row the objects to add to the table
     * @return the export itself for fluent method calls
     */
    public CSVExport addRow(List<?> row) {
        if (row != null) {
            addRow(row.toArray(new Object[row.size()]));
        }
        return this;
    }

}
