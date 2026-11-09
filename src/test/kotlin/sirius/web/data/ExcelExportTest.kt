/*
 * Made with all the love in the world
 * by scireum in Stuttgart, Germany
 *
 * Copyright by scireum GmbH
 * https://www.scireum.de - info@scireum.de
 */

package sirius.web.data

import org.apache.poi.ss.usermodel.Picture
import org.apache.poi.ss.usermodel.Workbook
import org.apache.poi.ss.usermodel.WorkbookFactory
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.extension.ExtendWith
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.ValueSource
import sirius.kernel.SiriusExtension
import sirius.kernel.commons.Amount
import sirius.kernel.commons.Files
import java.awt.image.BufferedImage
import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream
import java.math.BigDecimal
import java.time.LocalDate
import java.time.LocalDateTime
import javax.imageio.ImageIO
import kotlin.test.assertContentEquals
import kotlin.test.assertEquals

/**
 * Verifies the Excel files written by [ExcelExport].
 *
 * Besides round-tripping rows through the import, the export is reopened with POI itself where the
 * import cannot tell: the cell formats and embedded pictures a user sees in Excel.
 */
@ExtendWith(SiriusExtension::class)
class ExcelExportTest {

    @Test
    fun `ignores null inputs and does not write row`() {

        val testFile = File.createTempFile("excel-output", ".xlsx")

        val export = ExcelExport.asStandardXLSX()
        export.addListRow(null)
        export.addArrayRow("A-1", "B-1", "C-1")
        export.writeToStream(FileOutputStream(testFile))

        var expectLineNum = 1
        val expectedRow = listOf(listOf("A-1", "B-1", "C-1"))
        LineBasedProcessor.create(testFile.getName(), FileInputStream(testFile))
            .run(
                { lineNum, row ->
                    assertEquals(expectLineNum++, lineNum)
                    assertEquals(expectedRow[lineNum - 1], row.asList())
                },
                { _ -> false })

        Files.delete(testFile)
    }

    @Test
    fun `create simple excel sheet`() {

        val testFile = File.createTempFile("excel-output", ".xlsx")

        val export = ExcelExport.asStandardXLSX()
        export.addArrayRow("A-1", "B-1", "C-1")
        export.addListRow(listOf("A-2", "B-2", "C-2"))
        export.writeToStream(FileOutputStream(testFile))

        var expectLineNum = 1
        val expectedRow = listOf(listOf("A-1", "B-1", "C-1"), listOf("A-2", "B-2", "C-2"))
        LineBasedProcessor.create(testFile.getName(), FileInputStream(testFile))
            .run(
                { lineNum, row ->
                    assertEquals(expectLineNum++, lineNum)
                    assertEquals(expectedRow[lineNum - 1], row.asList())
                },
                { _ -> false })

        Files.delete(testFile)
    }

    @Test
    fun `create simple streaming excel sheet`() {

        val testFile = File.createTempFile("excel-output", ".xlsx")

        val export = ExcelExport.asStreamingXLSX()
        export.addArrayRow("A-1", "B-1", "C-1")
        export.addListRow(listOf("A-2", "B-2", "C-2"))
        export.writeToStream(FileOutputStream(testFile))

        var expectLineNum = 1
        val expectedRow = listOf(listOf("A-1", "B-1", "C-1"), listOf("A-2", "B-2", "C-2"))
        LineBasedProcessor.create(testFile.getName(), FileInputStream(testFile))
            .run(
                { lineNum, row ->
                    assertEquals(expectLineNum++, lineNum)
                    assertEquals(expectedRow[lineNum - 1], row.asList())
                },
                { _ -> false })

        Files.delete(testFile)
    }

    @Test
    fun `create excel sheet with multiple sheets`() {

        val testFile = File.createTempFile("excel-output", ".xlsx")
        val data =
            listOf(
                listOf("S1-A-1", "S1-B-1", "S1-C-1"),
                listOf("S1-A-2", "S1-B-2", "S1-C-2"),
                listOf("S2-A-1", "S2-B-1", "S2-C-1"),
                listOf("S2-A-2", "S2-B-2", "S2-C-2")
            )

        val export = ExcelExport.asStreamingXLSX(false)
        export.createSheet(null)
        export.addListRow(data[0])
        export.createSheet(null)
        export.addListRow(data[2])
        export.setCurrentSheet(0)
        export.addListRow(data[1])
        export.setCurrentSheet(1)
        export.addListRow(data[3])
        export.writeToStream(FileOutputStream(testFile))

        var currentData = 0
        XLSXProcessor.create(testFile.getName(), FileInputStream(testFile), true)
            .run(
                { _, row ->
                    assertEquals(data[currentData], row.asList())
                    currentData++
                },
                { _ -> false })

        Files.delete(testFile)
    }

    @ParameterizedTest
    @ValueSource(strings = ["xls", "xlsx", "streaming-xlsx"])
    fun `formats dates and timestamps as day-first dates`(format: String) {

        val export = exportFor(format)
        export.addArrayRow(LocalDate.of(2026, 1, 2), LocalDateTime.of(2026, 1, 2, 3, 4, 5))

        reopen(export).use { workbook ->
            val row = workbook.getSheetAt(0).getRow(0)
            assertEquals("dd.mm.yyyy", row.getCell(0).cellStyle.dataFormatString)
            assertEquals(LocalDateTime.of(2026, 1, 2, 0, 0, 0), row.getCell(0).localDateTimeCellValue)
            // the time of day is only hidden by the format, the cell still holds it
            assertEquals("dd.mm.yyyy", row.getCell(1).cellStyle.dataFormatString)
            assertEquals(LocalDateTime.of(2026, 1, 2, 3, 4, 5), row.getCell(1).localDateTimeCellValue)
        }
    }

    @ParameterizedTest
    @ValueSource(strings = ["xls", "xlsx", "streaming-xlsx"])
    fun `formats doubles, big decimals and amounts with two fraction digits but leaves ints and longs unformatted`(
        format: String
    ) {

        val export = exportFor(format)
        export.addArrayRow(3.5, BigDecimal("1234.567"), Amount.of(9.99), 42, 7L)

        reopen(export).use { workbook ->
            val row = workbook.getSheetAt(0).getRow(0)
            assertEquals(
                listOf("#,##0.00", "#,##0.00", "#,##0.00", "General", "General"),
                row.map { it.cellStyle.dataFormatString }
            )
            // the format only rounds the display, the cells keep every digit
            assertEquals(listOf(3.5, 1234.567, 9.99, 42.0, 7.0), row.map { it.numericCellValue })
        }
    }

    @ParameterizedTest
    @ValueSource(strings = ["xls", "xlsx", "streaming-xlsx"])
    fun `typed values survive a round trip through the import`(format: String) {

        val export = exportFor(format)
        export.addArrayRow("text", 42, 3.5, LocalDate.of(2026, 1, 2))

        val rows = mutableListOf<List<Any?>>()
        LineBasedProcessor.create("export.${extensionOf(format)}", ByteArrayInputStream(writeToBytes(export)))
            .run({ _, row -> rows.add(row.asList()) }, { false })

        assertEquals(listOf(listOf<Any?>("text", 42L, 3.5, LocalDateTime.of(2026, 1, 2, 0, 0, 0))), rows)
    }

    @ParameterizedTest
    @ValueSource(strings = ["xls", "xlsx", "streaming-xlsx"])
    fun `embeds an image cell as a picture scaled into its cell`(format: String) {

        val png = pngOf(200, 100)
        val export = exportFor(format)
        // the header row moves the image off row zero, so that a lost row anchor cannot pass for the default,
        // and its long title above the image would widen the column if autosizing ignored the picture
        export.addArrayRow("header", "a column title far wider than fifty pixels")
        export.addArrayRow("label", ExcelExport.ImageCell(png, 50, "image.png"))

        reopen(export).use { workbook ->
            val sheet = workbook.getSheetAt(0)
            val picture = sheet.drawingPatriarch.filterIsInstance<Picture>().single()
            assertEquals(Workbook.PICTURE_TYPE_PNG, picture.pictureData.pictureType)
            assertContentEquals(png, picture.pictureData.data)
            assertEquals(1, picture.clientAnchor.col1.toInt())
            assertEquals(1, picture.clientAnchor.row1)
            // the column is fixed to 50px in POI's width units (256 per 7px, plus an offset for the remainder)
            assertEquals(1828, sheet.getColumnWidth(1))
            // scaled to a quarter, the 200px wide image ends where its 50px column ends: unscaled, it would
            // reach far into the columns behind
            assertEquals(2, picture.clientAnchor.col2.toInt())
            // a quarter of 100px is 25px or 18.75pt, which ImageCell truncates to whole points
            assertEquals(18f, sheet.getRow(1).heightInPoints)
        }
    }

    private fun exportFor(format: String): ExcelExport = when (format) {
        "xls" -> ExcelExport.asXLS()
        "xlsx" -> ExcelExport.asStandardXLSX()
        "streaming-xlsx" -> ExcelExport.asStreamingXLSX()
        else -> throw IllegalArgumentException(format)
    }

    /**
     * Writes the given export and opens the result with POI, so that a test can inspect what Excel would show.
     */
    private fun reopen(export: ExcelExport): Workbook =
        WorkbookFactory.create(ByteArrayInputStream(writeToBytes(export)))

    private fun writeToBytes(export: ExcelExport): ByteArray {
        val bytes = ByteArrayOutputStream()
        export.writeToStream(bytes)
        return bytes.toByteArray()
    }

    private fun extensionOf(format: String) = if (format == "xls") "xls" else "xlsx"

    private fun pngOf(width: Int, height: Int): ByteArray {
        val bytes = ByteArrayOutputStream()
        ImageIO.write(BufferedImage(width, height, BufferedImage.TYPE_INT_RGB), "png", bytes)
        return bytes.toByteArray()
    }
}
