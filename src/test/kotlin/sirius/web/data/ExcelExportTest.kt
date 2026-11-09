/*
 * Made with all the love in the world
 * by scireum in Stuttgart, Germany
 *
 * Copyright by scireum GmbH
 * https://www.scireum.de - info@scireum.de
 */

package sirius.web.data

import org.apache.poi.ss.usermodel.Workbook
import org.apache.poi.ss.usermodel.WorkbookFactory
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.extension.ExtendWith
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.ValueSource
import sirius.kernel.SiriusExtension
import sirius.kernel.commons.Amount
import sirius.kernel.commons.Files
import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream
import java.math.BigDecimal
import java.time.LocalDate
import java.time.LocalDateTime
import kotlin.test.assertEquals

/**
 * Verifies the Excel files written by [ExcelExport].
 *
 * Besides round-tripping rows through the import, the export is reopened with POI itself where the
 * import cannot tell: the cell formats a user sees in Excel.
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
    fun `formats doubles, big decimals and amounts with two fraction digits but leaves ints and longs unformatted`(format: String) {

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

    private fun exportFor(format: String): ExcelExport = when (format) {
        "xls" -> ExcelExport.asXLS()
        "xlsx" -> ExcelExport.asStandardXLSX()
        "streaming-xlsx" -> ExcelExport.asStreamingXLSX()
        else -> throw IllegalArgumentException(format)
    }

    /**
     * Writes the given export and opens the result with POI, so that a test can inspect what Excel would show.
     */
    private fun reopen(export: ExcelExport): Workbook {
        val bytes = ByteArrayOutputStream()
        export.writeToStream(bytes)
        return WorkbookFactory.create(ByteArrayInputStream(bytes.toByteArray()))
    }
}
