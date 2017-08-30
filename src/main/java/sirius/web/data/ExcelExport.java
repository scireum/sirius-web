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
import org.apache.poi.hssf.usermodel.HSSFClientAnchor;
import org.apache.poi.hssf.usermodel.HSSFPatriarch;
import org.apache.poi.hssf.usermodel.HSSFPicture;
import org.apache.poi.hssf.usermodel.HSSFPrintSetup;
import org.apache.poi.hssf.usermodel.HSSFRichTextString;
import org.apache.poi.hssf.usermodel.HSSFRow;
import org.apache.poi.hssf.usermodel.HSSFSheet;
import org.apache.poi.hssf.usermodel.HSSFWorkbook;
import org.apache.poi.ss.usermodel.CellStyle;
import org.apache.poi.ss.usermodel.ClientAnchor;
import org.apache.poi.ss.usermodel.PrintSetup;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.ss.util.CellRangeAddress;
import sirius.kernel.health.Exceptions;
import sirius.kernel.nls.NLS;
import sirius.web.http.MimeHelper;
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
import java.util.HashSet;
import java.util.Set;

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
    private Set<Short> pictureCols = new HashSet<>();
    private HSSFPatriarch drawing;

    /**
     * Represents a header column for a column containing images. As images have a certain width, this sets the width
     * of
     * the column to the given value.
     * <p>
     * The column with has to be set during creation of the header as changing the column width later would result in
     * distorted images.
     */
    public static class ImageHeaderCol {

        private static final short EXCEL_COLUMN_WIDTH_FACTOR = 256;
        private static final int UNIT_OFFSET_LENGTH = 7;
        private static final int[] UNIT_OFFSET_MAP = {0, 36, 73, 109, 146, 182, 219};

        private String name;
        private int widthInPixel;

        /**
         * Creates a new header column object which can be used to create a column containing images. This has to be
         * inserted in the header (first row) only once per column as later insertions can result in distorted images.
         * <p>
         * The given width in pixels should be the width of the widest image to insert so that it fits in the column.
         *
         * @param name         the name of the column to print on the excel sheet
         * @param widthInPixel the width the column should have in pixels
         * @return a new image header column object
         */
        public static ImageHeaderCol create(String name, int widthInPixel) {
            ImageHeaderCol col = new ImageHeaderCol();
            col.name = name;
            col.widthInPixel = widthInPixel;
            return col;
        }

        /**
         * Returns width of column formatted in POI's unit for pixel width. This is not the exact pixel with but more of
         * an approximation.
         * <p>
         * Adapted from: https://stackoverflow.com/a/31837639
         *
         * @return column width formatted for POI
         */
        protected int getPOIWidth() {
            int widthUnits = EXCEL_COLUMN_WIDTH_FACTOR * (widthInPixel / UNIT_OFFSET_LENGTH);
            widthUnits += UNIT_OFFSET_MAP[(widthInPixel % UNIT_OFFSET_LENGTH)];
            return widthUnits / 2;
        }
    }

    /**
     * Represents a cell containing an image which should be inserted.
     */
    public static class ImageCell {
        byte[] fileData;
        int heightInPixel;
        int pictureType;

        /**
         * Creates a new cell containing an image which could be inserted into an Excel sheet.
         * <p>
         * The height in pixel determines the height of the row containing the image. Currently, only one image is
         * allowed per row as the changes in height for the row might result in distorted images.
         * <p>
         * The filename is needed in order to determine the type of image. POI requires an image type for inserting
         * images. Only jpeg, png and bmp are supported.
         *
         * @param fileData      the data of the picture
         * @param heightInPixel the height the row containing the image should have
         * @param fileName      the filename or path to file containing the filename
         * @return a new image cell object
         */
        public static ImageCell create(byte[] fileData, int heightInPixel, String fileName) {
            ImageCell cell = new ImageCell();
            int guessedPictureType = determinePictureType(fileName);
            if (guessedPictureType < 0) {
                throw Exceptions.handle()
                                .withSystemErrorMessage("Unknown picture type for file %s: %s (%s)", fileName)
                                .handle();
            }
            cell.pictureType = guessedPictureType;
            cell.fileData = fileData;
            cell.heightInPixel = heightInPixel;
            return cell;
        }

        /**
         * Converted height in pixels of row containing the image as POI uses points as unit for row heights.
         *
         * @return the height the row should have in points
         */
        protected int getHeightInPoints() {
            return heightInPixel * 72 / 96;
        }

        private static int determinePictureType(String fileName) {
            String mimeType = MimeHelper.guessMimeType(fileName);
            switch (mimeType) {
                case MimeHelper.IMAGE_PNG:
                    return Workbook.PICTURE_TYPE_PNG;
                case MimeHelper.IMAGE_JPEG:
                    return Workbook.PICTURE_TYPE_JPEG;
                case "image/bmp":
                    return Workbook.PICTURE_TYPE_DIB;
                default:
                    return -1;
            }
        }
    }

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
        if (obj instanceof ImageHeaderCol) {
            cell.setCellValue(new HSSFRichTextString(((ImageHeaderCol) obj).name));
            sheet.setColumnWidth(columnIndex, ((ImageHeaderCol) obj).getPOIWidth());
            pictureCols.add((short) columnIndex);
            return;
        }
        if (obj instanceof ImageCell) {
            addImageCell(row, (ImageCell) obj, columnIndex);
            return;
        }
        cell.setCellValue(new HSSFRichTextString(obj.toString()));
    }

    private void addImageCell(HSSFRow row, ImageCell imageCell, int columnIndex) {
        row.setHeightInPoints(imageCell.getHeightInPoints());
        if (drawing == null) {
            drawing = sheet.createDrawingPatriarch();
        }
        int iconIndex = workbook.addPicture(imageCell.fileData, imageCell.pictureType);
        ClientAnchor anchor = new HSSFClientAnchor();
        anchor.setCol1(columnIndex);
        anchor.setRow1(row.getRowNum());
        HSSFPicture picture = drawing.createPicture(anchor, iconIndex);
        picture.resize();
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
                    // Don't distort images
                    if (!pictureCols.contains(col)) {
                        sheet.autoSizeColumn(col);
                    }
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
