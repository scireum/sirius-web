/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.data;

import com.monitorjbl.xlsx.StreamingReader;
import org.apache.poi.hssf.usermodel.HSSFWorkbook;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellType;
import org.apache.poi.ss.usermodel.DateUtil;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import sirius.kernel.async.TaskContext;
import sirius.kernel.commons.Doubles;
import sirius.kernel.commons.RateLimit;
import sirius.kernel.commons.Strings;
import sirius.kernel.commons.Values;
import sirius.kernel.nls.NLS;

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
public class XLSProcessor implements LineBasedProcessor {

    private InputStream input;
    private boolean xslx;

    XLSProcessor(InputStream input, boolean xslx) {
        super();
        this.input = input;
        this.xslx = xslx;
    }

    @Override
    public void run(RowProcessor rowProcessor, Predicate<Exception> errorHandler) throws Exception {
        Workbook wb = xslx ? StreamingReader.builder().bufferSize(4096).open(input) : new HSSFWorkbook(input);
        Sheet sheet = wb.getSheetAt(0);
        Iterator<Row> iter = sheet.rowIterator();
        int current = 0;
        TaskContext tc = TaskContext.get();
        RateLimit stateUpdateLimiter = tc.shouldUpdateState();

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
                if (stateUpdateLimiter.check()) {
                    tc.setState(NLS.get("LineBasedProcessor.linesProcessed"), current);
                }
            } catch (Exception e) {
                if (!errorHandler.test(e)) {
                    throw e;
                }
            }
        }

        if (tc.isActive() && current > 0) {
            tc.setState(NLS.get("LineBasedProcessor.linesProcessed"), current);
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
        if (cellType == CellType.BLANK) {
            return null;
        }
        throw new IllegalArgumentException(Strings.apply(
                "Cannot read a value of type %d from cell at row %d, column  %d",
                cellType,
                cell.getRowIndex(),
                cell.getColumnIndex()));
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
