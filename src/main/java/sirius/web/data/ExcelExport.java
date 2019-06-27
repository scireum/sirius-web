/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.data;

import io.netty.handler.codec.http.HttpResponseStatus;
import org.apache.poi.hssf.usermodel.HSSFClientAnchor;
import org.apache.poi.hssf.usermodel.HSSFRichTextString;
import org.apache.poi.hssf.usermodel.HSSFWorkbook;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellStyle;
import org.apache.poi.ss.usermodel.ClientAnchor;
import org.apache.poi.ss.usermodel.Drawing;
import org.apache.poi.ss.usermodel.Picture;
import org.apache.poi.ss.usermodel.PrintSetup;
import org.apache.poi.ss.usermodel.RichTextString;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.ss.util.CellRangeAddress;
import org.apache.poi.xssf.usermodel.XSSFClientAnchor;
import org.apache.poi.xssf.usermodel.XSSFRichTextString;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import sirius.kernel.commons.Amount;
import sirius.kernel.health.Exceptions;
import sirius.kernel.nls.NLS;
import sirius.web.http.MimeHelper;
import sirius.web.http.WebContext;

import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
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

    private static final String MIME_TYPE_XLS = "application/ms-excel";
    private static final String MIME_TYPE_XLSX = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet";

    private final Workbook workbook;
    private final Sheet sheet;
    private int rows = 0;
    private int maxCols = 0;
    private CellStyle dateStyle;
    private CellStyle numeric;
    private CellStyle borderStyle;
    private CellStyle normalStyle;
    private Set<Short> pictureCols = new HashSet<>();
    private Drawing drawing;

    /**
     * Represents a cell containing an image which should be inserted.
     */
    public static class ImageCell {

        private static final short EXCEL_COLUMN_WIDTH_FACTOR = 256;
        private static final int[] UNIT_OFFSET_ARRAY = {0, 36, 73, 109, 146, 182, 219};

        byte[] fileData;
        int heightInPixel;
        int widthInPixel;
        int colWidthInPixel;
        int pictureType;

        /**
         * Creates a new cell containing an image which could be inserted into an Excel sheet.
         * <p>
         * The height of the image determines the height of the row containing the image. Currently, only one image is
         * allowed per row as the changes in height for the row might result in distorted images.
         * <p>
         * Images are scaled to fit in a cell. Their scaling is determined by their width and the parameter
         * colWidthInPixel. Images are only scaled down but never up.
         * <p>
         * Only the first image cell inserted in a given column is able to set the width of the column
         * as changing a column's width while images being present in the column would result in them beind distorted.
         * <p>
         * The filename is needed in order to determine the type of image. POI requires an image type for inserting
         * images. Only jpeg, png and bmp are supported.
         *
         * @param fileData        the data of the picture
         * @param colWidthInPixel the width of the column containing the image should have
         * @param fileName        the filename or path to file containing the filename
         */
        public ImageCell(byte[] fileData, int colWidthInPixel, String fileName) {
            int guessedPictureType = determinePictureType(fileName);
            if (guessedPictureType < 0) {
                throw Exceptions.handle()
                                .withSystemErrorMessage("Unknown picture type for file %s: %s (%s)", fileName)
                                .handle();
            }
            this.pictureType = guessedPictureType;
            this.fileData = fileData;
            this.colWidthInPixel = colWidthInPixel;
            try {
                determineImageSize(fileData);
            } catch (IOException e) {
                throw Exceptions.handle(e);
            }
        }

        private void determineImageSize(byte[] fileData) throws IOException {
            BufferedImage bufferedImage = ImageIO.read(new ByteArrayInputStream(fileData));
            this.heightInPixel = bufferedImage.getHeight();
            this.widthInPixel = bufferedImage.getWidth();
        }

        /**
         * Converted height in pixels of row containing the image as POI uses points as unit for row heights.
         *
         * @return the height the row should have in points
         */
        protected int getHeightInPoints() {
            return (int) ((heightInPixel * 72f / 96f) * getScaleFactor());
        }

        /**
         * Returns width of column containing the image formatted in POI's unit for pixel width. This is not the exact
         * pixel with but more of an approximation.
         * <p>
         * Adapted from: https://stackoverflow.com/a/31837639
         *
         * @return column width formatted for POI
         */
        protected int getPOIWidth() {
            int widthUnits = EXCEL_COLUMN_WIDTH_FACTOR * (colWidthInPixel / UNIT_OFFSET_ARRAY.length);
            widthUnits += UNIT_OFFSET_ARRAY[colWidthInPixel % UNIT_OFFSET_ARRAY.length];
            return widthUnits;
        }

        /**
         * Calculates the scale factor images should be scaled to so that they fit into the set column width. Images
         * with a width smaller than the column width are not scaled up but keep their size (scaling of 100%).
         *
         * @return the scaling factor of images to fit in the column
         */
        protected float getScaleFactor() {
            float scaleFactor = (float) colWidthInPixel / widthInPixel;
            if (scaleFactor > 1f) {
                return 1f;
            }
            return scaleFactor;
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
     * @deprecated Use one of the static constructor methods to determine which format to use.
     */
    @Deprecated
    public ExcelExport() {
        this(true);
    }

    protected ExcelExport(boolean useHSSF) {
        workbook = useHSSF ? new HSSFWorkbook() : new XSSFWorkbook();
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
        PrintSetup ps = sheet.getPrintSetup();
        ps.setPaperSize(PrintSetup.A4_PAPERSIZE);
        ps.setLandscape(false);
        ps.setFitWidth((short) 1);
        ps.setFitHeight((short) 0);
        sheet.setAutobreaks(true);
        sheet.setRepeatingRows(new CellRangeAddress(0, 0, -1, -1));
    }

    /**
     * Creates a new export which uses the legacy Excel'97 format (.xls).
     *
     * @return a new exporter using the Excel'97 format
     */
    public static ExcelExport asXLS() {
        return new ExcelExport(true);
    }

    /**
     * Creates a new export which uses the modern Excel format (.xlsx).
     *
     * @return a new exporter using the modern Excel format
     */
    public static ExcelExport asXSLX() {
        return new ExcelExport(false);
    }

    private void addCell(Row row, Object obj, int columnIndex, CellStyle style) {
        if (obj == null) {
            return;
        }
        Cell cell = row.createCell(columnIndex);
        cell.setCellStyle(style);
        if (obj instanceof String) {
            cell.setCellValue(createRichTextString((String) obj));
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
            cell.setCellValue(createRichTextString(NLS.toUserString(obj)));
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
        if (obj instanceof Amount && ((Amount) obj).isFilled()) {
            cell.setCellValue(((Amount) obj).getAmount().doubleValue());
            return;
        }
        if (obj instanceof ImageCell) {
            addImageCell(row, (ImageCell) obj, columnIndex);
            return;
        }
        cell.setCellValue(createRichTextString(obj.toString()));
    }

    private RichTextString createRichTextString(String string) {
        if (workbook instanceof HSSFWorkbook) {
            return new HSSFRichTextString(string);
        }

        return new XSSFRichTextString(string);
    }

    private void addImageCell(Row row, ImageCell imageCell, int columnIndex) {
        row.setHeightInPoints(imageCell.getHeightInPoints());
        if (drawing == null) {
            drawing = sheet.createDrawingPatriarch();
        }
        if (!pictureCols.contains((short) columnIndex)) {
            sheet.setColumnWidth(columnIndex, imageCell.getPOIWidth());
            pictureCols.add((short) columnIndex);
        }
        int iconIndex = workbook.addPicture(imageCell.fileData, imageCell.pictureType);

        ClientAnchor anchor;

        if (workbook instanceof HSSFWorkbook) {
            anchor = new HSSFClientAnchor();
        } else {
            anchor = new XSSFClientAnchor();
        }

        anchor.setCol1(columnIndex);
        anchor.setRow1(row.getRowNum());
        Picture picture = drawing.createPicture(anchor, iconIndex);
        picture.resize(imageCell.getScaleFactor());
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
            Row r = sheet.createRow(rows++);
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
        String filename = computeEffectiveFilename(name);
        OutputStream out = ctx.respondWith()
                              .download(filename)
                              .notCached()
                              .outputStream(HttpResponseStatus.OK,
                                            workbook instanceof HSSFWorkbook ? MIME_TYPE_XLS : MIME_TYPE_XLSX);
        writeToStream(out);
    }

    private String computeEffectiveFilename(String name) {
        if (!name.contains(".")) {
            if (workbook instanceof HSSFWorkbook) {
                return name + ".xls";
            } else {
                return name + ".xlsx";
            }
        }
        return name;
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
                autosizeColumns();

                // Add autofilter...
                sheet.setAutoFilter(new CellRangeAddress(0, rows, 0, maxCols - 1));
                workbook.write(out);
            }
        } catch (IOException e) {
            throw Exceptions.handle(e);
        }
    }

    private void autosizeColumns() {
        for (short col = 0; col < maxCols; col++) {
            // Don't distort images
            if (!pictureCols.contains(col)) {
                sheet.autoSizeColumn(col);
            }
        }
    }

    private CellStyle getCellStyleForObject(Object data) {
        if (data instanceof LocalDate || data instanceof LocalDateTime) {
            return dateStyle;
        }
        if (isNumeric(data)) {
            return numeric;
        }
        return normalStyle;
    }

    private boolean isNumeric(Object data) {
        if (data instanceof Integer || data instanceof Double || data instanceof Long || data instanceof BigDecimal) {
            return true;
        }

        return (data instanceof Amount) && ((Amount) data).isFilled();
    }
}
