/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.data;

import com.github.pjfanning.xlsx.StreamingReader;
import org.apache.poi.ss.usermodel.Workbook;

import java.io.IOException;
import java.io.InputStream;

/**
 * In charge of processing XLSX (MS Excel) files.
 */
public class XLSXProcessor extends XLSProcessor {

    /**
     * Creates a new processor for XLSX (MS Excel) files.
     *
     * @param input           the stream of rows to be processed
     * @param importAllSheets true, if all sheets should be processed, false otherwise
     */
    public XLSXProcessor(InputStream input, boolean importAllSheets) {
        super(input, importAllSheets);
    }

    @Override
    public Workbook openWorkbook() throws IOException {
        return StreamingReader.builder().bufferSize(8192).open(input);
    }
}
