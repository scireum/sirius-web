/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.data;

import com.github.pjfanning.xlsx.exceptions.MissingSheetException;
import org.apache.poi.hssf.usermodel.HSSFWorkbook;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellType;
import org.apache.poi.ss.usermodel.DateUtil;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import sirius.kernel.async.TaskContext;
import sirius.kernel.commons.Doubles;
import sirius.kernel.commons.Strings;
import sirius.kernel.commons.Values;
import sirius.kernel.health.Exceptions;
import sirius.kernel.nls.NLS;

import java.io.IOException;
import java.io.InputStream;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.function.Predicate;

/**
 * In charge of processing XLS (MS Excel) files.
 */
public class XLSProcessor extends LineBasedProcessor {

    protected final InputStream input;
    protected final boolean importAllSheets;

    /**
     * Creates a new processor for XLS (MS Excel) files.
     *
     * @param input           the stream of rows to be processed
     * @param importAllSheets true, if all sheets should be processed, false otherwise
     */
    public XLSProcessor(InputStream input, boolean importAllSheets) {
        super();
        this.input = input;
        this.importAllSheets = importAllSheets;
    }

    @Override
    public void run(RowProcessor rowProcessor, Predicate<Exception> errorHandler) throws Exception {
        try (Workbook wb = openWorkbook()) {
            if (importAllSheets) {
                for (int i = 0; i < wb.getNumberOfSheets(); i++) {
                    importSheet(rowProcessor, errorHandler, wb.getSheetAt(i));
                }
            } else {
                importSheet(rowProcessor, errorHandler, wb.getSheetAt(0));
            }
        }
    }

    /**
     * Processes the XLS (MS Excel) file using the given {@link SheetBasedRowProcessor sheetProcessors}.
     *
     * @param sheetProcessors one or more sheet processors to use
     * @throws Exception                             in case an error occurred while processing.
     * @throws IOException                           if the stream providing the given workbook cannot be read.
     * @throws sirius.kernel.health.HandledException if workbook does not contain a sheet with the given name.
     */
    public void runForSheets(SheetBasedRowProcessor... sheetProcessors) throws Exception {
        try (Workbook wb = openWorkbook()) {
            for (SheetBasedRowProcessor processor : sheetProcessors) {
                runForSheet(wb, processor);
            }
        }
    }

    private void runForSheet(Workbook workbook, SheetBasedRowProcessor processor) throws Exception {
        try {
            Sheet sheet = workbook.getSheet(processor.getSheetName());
            importSheet(processor.getRowProcessor(), processor.getErrorHandler(), sheet);
        } catch (MissingSheetException missingSheetException) {
            throw Exceptions.createHandled()
                            .withNLSKey("XLSProcessor.error.missingSheet")
                            .set("sheet", processor.getSheetName())
                            .handle();
        }
    }

    protected Workbook openWorkbook() throws IOException {
        return new HSSFWorkbook(input);
    }

    protected void importSheet(RowProcessor rowProcessor, Predicate<Exception> errorHandler, Sheet sheet) {
        Iterator<Row> iter = sheet.rowIterator();
        int current = 0;
        TaskContext tc = TaskContext.get();

        while (iter.hasNext() && tc.isActive()) {
            try {
                current++;
                Row row = iter.next();
                short first = 0;
                short last = getLastFilledCell(row);
                List<Object> values = new ArrayList<>();
                for (int i = first; i <= last; i++) {
                    Cell cell = row.getCell(i);
                    Object value = extractCellValue(cell);
                    values.add(value);
                }
                rowProcessor.handleRow(current, Values.of(values));
                tc.tryUpdateState(NLS.get("LineBasedProcessor.linesProcessed"), current);
            } catch (Exception e) {
                if (!errorHandler.test(e)) {
                    throw e;
                }
            }
        }

        if (tc.isActive() && current > 0) {
            tc.forceUpdateState(NLS.get("LineBasedProcessor.linesProcessed"), current);
        }
    }

    private short getLastFilledCell(Row row) {
        short lastFilled = row.getLastCellNum();
        while (lastFilled > -1 && Strings.isEmpty(extractCellValue(row.getCell(lastFilled)))) {
            lastFilled--;
        }
        return lastFilled;
    }

    private Object extractCellValue(Cell cell) {
        if (cell == null) {
            return null;
        }

        CellType cellType = cell.getCellType();
        if (cellType == CellType.FORMULA) {
            cellType = cell.getCachedFormulaResultType();
        }
        if (cellType == CellType.BOOLEAN) {
            return cell.getBooleanCellValue();
        }
        if (cellType == CellType.NUMERIC) {
            return extractNumericValue(cell);
        }
        if (cellType == CellType.STRING) {
            return extractStringValue(cell);
        }
        if (cellType == CellType.BLANK || cellType == CellType.ERROR) {
            return null;
        }
        throw new IllegalArgumentException(NLS.fmtr("XLSProcessor.error.invalidValueInCell")
                                              .set("cellType", cellType)
                                              .set("sheet", cell.getSheet().getSheetName())
                                              .set("row", cell.getRowIndex())
                                              .set("column", cell.getColumnIndex())
                                              .format());
    }

    private Object extractStringValue(Cell cell) {
        String value = cell.getRichStringCellValue().getString();
        if (value != null) {
            return value.trim();
        } else {
            return null;
        }
    }

    private Object extractNumericValue(Cell cell) {
        if (DateUtil.isCellDateFormatted(cell)) {
            Date dateCellValue = cell.getDateCellValue();
            if (dateCellValue == null) {
                return null;
            }
            return LocalDateTime.ofInstant(dateCellValue.toInstant(), ZoneId.systemDefault());
        }
        double val = cell.getNumericCellValue();
        if (Doubles.isZero(Doubles.frac(val))) {
            return Math.round(val);
        } else {
            return val;
        }
    }
}
