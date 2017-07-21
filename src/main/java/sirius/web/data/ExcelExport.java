/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.data;

import io.netty.handler.codec.http.HttpResponseStatus;
import org.apache.poi.hssf.usermodel.HSSFCell;
import org.apache.poi.hssf.usermodel.HSSFCellStyle;
import org.apache.poi.hssf.usermodel.HSSFPrintSetup;
import org.apache.poi.hssf.usermodel.HSSFRichTextString;
import org.apache.poi.hssf.usermodel.HSSFRow;
import org.apache.poi.hssf.usermodel.HSSFSheet;
import org.apache.poi.hssf.usermodel.HSSFWorkbook;
import org.apache.poi.ss.usermodel.CellStyle;
import org.apache.poi.ss.usermodel.PrintSetup;
import org.apache.poi.ss.util.CellRangeAddress;
import sirius.kernel.health.Exceptions;
import sirius.kernel.nls.NLS;
import sirius.web.http.WebContext;

import java.io.IOException;
import java.io.OutputStream;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;

/**
 * Generates an Excel file which can be sent as a response for a {@link sirius.web.http.WebContext}
 */
public class ExcelExport {

    private static final String MIME_TYPE_EXCEL = "application/ms-excel";

    private final HSSFWorkbook workbook;
    private final HSSFSheet sheet;
    private int rows = 0;
    private int maxCols = 0;
    private HSSFCellStyle dateStyle;
    private HSSFCellStyle numeric;
    private HSSFCellStyle borderStyle;
    private HSSFCellStyle normalStyle;

    /**
     * Generates a new Export
     */
    public ExcelExport() {
        workbook = new HSSFWorkbook();
        sheet = workbook.createSheet();
        // Setup styles
        dateStyle = workbook.createCellStyle();
        dateStyle.setDataFormat(workbook.createDataFormat().getFormat("dd.mm.yyyy"));
        numeric = workbook.createCellStyle();
        numeric.setDataFormat(workbook.createDataFormat().getFormat("#,##0.00"));

        borderStyle = workbook.createCellStyle();
        borderStyle.setBorderBottom(CellStyle.BORDER_THICK);
        normalStyle = workbook.createCellStyle();
        // Setup layout
        sheet.createFreezePane(0, 1, 0, 1);
        HSSFPrintSetup ps = sheet.getPrintSetup();
        ps.setPaperSize(PrintSetup.A4_PAPERSIZE);
        ps.setLandscape(false);
        ps.setFitWidth((short) 1);
        ps.setFitHeight((short) 0);
        sheet.setAutobreaks(true);
        sheet.setRepeatingRows(new CellRangeAddress(0, 0, -1, -1));
    }

    private void addCell(HSSFRow row, Object obj, int columnIndex, HSSFCellStyle style) {
        if (obj == null) {
            return;
        }
        HSSFCell cell = row.createCell(columnIndex);
        cell.setCellStyle(style);
        if (obj instanceof String) {
            cell.setCellValue(new HSSFRichTextString((String) obj));
            return;
        }
        if (obj instanceof LocalDateTime) {
            cell.setCellValue(Date.from(((LocalDateTime) obj).atZone(ZoneId.systemDefault()).toInstant()));
            return;
        }
        if (obj instanceof LocalDate) {
            cell.setCellValue(Date.from(((LocalDate) obj).atStartOfDay(ZoneId.systemDefault()).toInstant()));
            return;
        }
        if (obj instanceof Boolean) {
            cell.setCellValue(new HSSFRichTextString(NLS.toUserString(obj)));
            return;
        }
        if (obj instanceof Double) {
            cell.setCellValue((Double) obj);
            return;
        }
        if (obj instanceof Float) {
            cell.setCellValue((Float) obj);
            return;
        }
        if (obj instanceof Integer) {
            cell.setCellValue((Integer) obj);
            return;
        }
        if (obj instanceof Long) {
            cell.setCellValue((Long) obj);
            return;
        }
        if (obj instanceof BigDecimal) {
            cell.setCellValue(((BigDecimal) obj).doubleValue());
            return;
        }
        cell.setCellValue(new HSSFRichTextString(obj.toString()));
    }

    /**
     * Adds the given array of objects as a row.
     *
     * @param row the objects to add to the table
     * @return the export itself for fluent method calls
     */
    public ExcelExport addRow(Object... row) {
        addRowAsList(Arrays.asList(row));
        return this;
    }

    /**
     * Adds the given collection of objects as a row.
     *
     * @param row the objects to add to the table
     * @return the export itself for fluent method calls
     */
    public ExcelExport addRowAsList(Collection<?> row) {
        if (row != null) {
            maxCols = Math.max(maxCols, row.size());
            int idx = 0;
            HSSFRow r = sheet.createRow(rows++);
            for (Object entry : row) {
                addCell(r, entry, idx++, getCellStyleForObject(entry));
            }
        }
        return this;
    }

    /**
     * Writes the generated Excel file to the given web context.
     *
     * @param name the filename to use
     * @param ctx  the target context to create a response for
     */
    public void writeResponseTo(String name, WebContext ctx) {
        OutputStream out =
                ctx.respondWith().download(name).notCached().outputStream(HttpResponseStatus.OK, MIME_TYPE_EXCEL);
        writeToStream(out);
    }

    /**
     * Writes the generated excel file to the given stream.
     *
     * @param stream the target stream to write the excel workbook to
     */
    public void writeToStream(OutputStream stream) {
        try {
            try (OutputStream out = stream) {
                // Make it pretty...
                for (short col = 0; col < maxCols; col++) {
                    sheet.autoSizeColumn(col);
                }
                // Add autofilter...
                sheet.setAutoFilter(new CellRangeAddress(0, rows, 0, maxCols - 1));
                workbook.write(out);
            }
        } catch (IOException e) {
            throw Exceptions.handle(e);
        }
    }

    private HSSFCellStyle getCellStyleForObject(Object data) {
        HSSFCellStyle style = normalStyle;
        if (data instanceof LocalDate || data instanceof LocalDateTime) {
            style = dateStyle;
        } else if (data instanceof Integer || data instanceof Double || data instanceof Long) {
            style = numeric;
        }
        return style;
    }
}
