/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates;

import com.google.common.base.Charsets;
import com.google.common.collect.Lists;
import org.apache.poi.hssf.usermodel.HSSFCell;
import org.apache.poi.hssf.usermodel.HSSFWorkbook;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import sirius.kernel.async.TaskContext;
import sirius.kernel.commons.BOMReader;
import sirius.kernel.commons.CSVReader;
import sirius.kernel.commons.Doubles;
import sirius.kernel.commons.Strings;
import sirius.kernel.commons.Values;
import sirius.kernel.commons.Watch;
import sirius.kernel.health.Exceptions;
import sirius.kernel.nls.NLS;

import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * Processes line based input files like MS Excel or CSV.
 */
public abstract class LineBasedProcessor {

    /**
     * Generates an appropriate LineBasedProcessor based on the file extension of the given file.
     *
     * @param name  the name of the file to process
     * @param input an input stream containing the data to import
     * @return an appropriate processor for the given file
     * @throws sirius.kernel.health.HandledException if no processor can handle the given file
     */
    public static LineBasedProcessor create(String name, InputStream input) {
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
     * @throws Exception in case an error occurred while processing.
     */
    public abstract void run(RowProcessor rowProcessor) throws Exception;

    /**
     * In charge of processing XLS (MS Excel) files.
     */
    protected static class XLSProcessor extends LineBasedProcessor {
        private InputStream input;
        private boolean xslx;

        public XLSProcessor(InputStream input, boolean xslx) {
            super();
            this.input = input;
            this.xslx = xslx;
        }

        @Override
        public void run(RowProcessor rowProcessor) throws Exception {
            Workbook wb = xslx ? new XSSFWorkbook(input) : new HSSFWorkbook(input);
            Sheet sheet = wb.getSheetAt(0);
            Iterator<Row> iter = sheet.rowIterator();
            int current = 0;
            TaskContext tc = TaskContext.get();
            while (iter.hasNext() && tc.isActive()) {
                Watch w = Watch.start();
                current++;
                Row row = iter.next();
                short first = 0;
                short last = row.getLastCellNum();
                List<Object> values = Lists.newArrayList();
                for (int i = first; i <= last; i++) {
                    Cell cell = row.getCell(i);
                    Object value = extractCellValue(cell);
                    values.add(value);
                }
                rowProcessor.handleRow(current, Values.of(values));
                tc.setState(NLS.get("LineBasedProcessor.linesProcessed"), current);
            }
        }

        private Object extractCellValue(Cell cell) {
            if (cell == null) {
                return null;
            }
            int cellType = cell.getCellType();
            if (cellType == HSSFCell.CELL_TYPE_FORMULA) {
                cellType = cell.getCachedFormulaResultType();
            }
            if (cellType == HSSFCell.CELL_TYPE_BOOLEAN) {
                return cell.getBooleanCellValue();
            }

            if (cellType == HSSFCell.CELL_TYPE_NUMERIC) {
                double val = cell.getNumericCellValue();
                if (Doubles.isZero(Doubles.frac(val))) {
                    return Math.round(val);
                } else {
                    return val;
                }
            }

            if (cellType == HSSFCell.CELL_TYPE_STRING) {
                String value = cell.getRichStringCellValue().getString();
                if (value != null) {
                    return value.trim();
                } else {
                    return null;
                }
            }
            throw new IllegalArgumentException(Strings.apply(
                    "Cannot read a value of type %d from cell at row %d, column  %d",
                    cellType,
                    cell.getRowIndex(),
                    cell.getColumnIndex()));
        }
    }

    /**
     * In charge of processing CSV (comma separated values) files.
     */
    private static class CSVProcessor extends LineBasedProcessor {
        private InputStream input;

        private CSVProcessor(InputStream input) {
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

    /**
     * Invoked by a LineBasedProcessor to handle one row.
     */
    public interface RowProcessor {
        /**
         * Called the handle a row of an input file.
         * <p>
         * Note that an exception thrown here will completely abort the whole process. Therefore proper exception
         * handling is required.
         * </p>
         *
         * @param lineNumber the line number which is currently being processed.
         * @param row        the data of the row / line to process
         */
        void handleRow(int lineNumber, Values row);
    }
}
