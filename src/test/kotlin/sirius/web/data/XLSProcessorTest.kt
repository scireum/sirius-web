/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.data

import org.apache.poi.hssf.usermodel.HSSFWorkbook
import org.apache.poi.ss.usermodel.Cell
import org.apache.poi.ss.usermodel.CellStyle
import org.apache.poi.ss.usermodel.CellType
import org.apache.poi.ss.usermodel.Row
import org.apache.poi.ss.usermodel.Sheet
import org.apache.poi.ss.usermodel.Workbook
import org.apache.poi.xssf.usermodel.XSSFWorkbook
import org.junit.jupiter.api.extension.ExtendWith
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.ValueSource
import sirius.kernel.SiriusExtension
import sirius.kernel.commons.Values
import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.time.LocalDateTime
import java.time.ZoneId
import java.util.Date
import java.util.TimeZone
import java.util.function.Predicate
import kotlin.test.assertEquals
import kotlin.test.assertFailsWith

/**
 * Verifies that [XLSProcessor] and [XLSXProcessor] read the same file contents in the same way.
 *
 * Both processors share their cell extraction, but obtain their workbook from entirely different
 * libraries - POI's `HSSFWorkbook` for XLS and the streaming reader for XLSX - so every expectation
 * here is asserted against both formats to keep the two backends from drifting apart.
 *
 * The workbooks are written by POI at the start of each test rather than read from a committed binary,
 * so that the contents being asserted can be reviewed right next to the assertions. Files as they are
 * written by an actual spreadsheet application are covered by `LineBasedProcessorTest`, whose
 * `test.xlsx` was saved by Excel.
 */
@ExtendWith(SiriusExtension::class)
class XLSProcessorTest {

    @ParameterizedTest
    @ValueSource(strings = ["xls", "xlsx"])
    fun `reads strings and numbers`(format: String) {

        val rows = readCellTypes(format)

        assertEquals(listOf("padded", "plain", "umlauts äöüß"), rows[0])
        assertEquals(listOf(42L, 3.5, -7L, 0L), rows[1])
    }

    @ParameterizedTest
    @ValueSource(strings = ["xls", "xlsx"])
    fun `reads dates using the default time zone`(format: String) {

        // the zone is pinned because the extraction converts via java.util.Date and back: on a JVM
        // running in UTC the two conversions cancel out and the assertion would hold even if the
        // extraction stopped honouring the default zone
        val rows = withTimeZone("Europe/Berlin") { readCellTypes(format) }

        assertEquals(
            listOf(LocalDateTime.of(2026, 1, 2, 0, 0, 0), LocalDateTime.of(2026, 1, 2, 3, 4, 5)),
            rows[2]
        )
    }

    @ParameterizedTest
    @ValueSource(strings = ["xls", "xlsx"])
    fun `reads boolean cells`(format: String) {

        assertEquals(listOf(true, false), readCellTypes(format)[3])
    }

    @ParameterizedTest
    @ValueSource(strings = ["xls", "xlsx"])
    fun `reads formulas via their cached results`(format: String) {

        // the error formula (1 divided by 0) is read as null and only survives because the formulas
        // behind it keep it from being trimmed away as a trailing empty cell
        assertEquals(listOf(null, 7L, "ab", true), readCellTypes(format)[4])
    }

    @ParameterizedTest
    @ValueSource(strings = ["xls", "xlsx"])
    fun `keeps gaps within a row but drops trailing empty cells`(format: String) {

        assertEquals(listOf("start", null, "after gap"), readCellTypes(format)[5])
    }

    @ParameterizedTest
    @ValueSource(strings = ["xls", "xlsx"])
    fun `anchors every row at the first column`(format: String) {

        // the row starts in column C, which must arrive as two leading nulls rather than shifting left
        assertEquals(listOf(null, null, "leading gap"), readCellTypes(format)[9])
    }

    @ParameterizedTest
    @ValueSource(strings = ["xls", "xlsx"])
    fun `reports a row holding nothing but whitespace as empty`(format: String) {

        assertEquals(emptyList(), readCellTypes(format)[6])
    }

    @ParameterizedTest
    @ValueSource(strings = ["xls", "xlsx"])
    fun `reports a row without a single cell as empty`(format: String) {

        assertEquals(emptyList(), readCellTypes(format)[7])
    }

    @ParameterizedTest
    @ValueSource(strings = ["xls", "xlsx"])
    fun `numbers the processed rows instead of the spreadsheet rows`(format: String) {

        val rows = readNumberedRows(format, ::fillCellTypes)

        // the workbook has no cell in the spreadsheet rows 9 and 10, which must not shift the numbering
        assertEquals((1..10).toList(), rows.map { it.first })
        assertEquals(listOf("after skipped rows"), rows[8].second)
    }

    @ParameterizedTest
    @ValueSource(strings = ["xls", "xlsx"])
    fun `keeps importing when the error handler swallows a failure`(format: String) {

        val handled = mutableListOf<Exception>()
        val lineNumbers = mutableListOf<Int>()

        process(format, ::fillCellTypes, false, { exception -> handled.add(exception); true }) { lineNumber, _ ->
            if (lineNumber == 2) {
                throw IllegalStateException("Row 2 cannot be imported")
            }
            lineNumbers.add(lineNumber)
        }

        assertEquals(1, handled.size)
        assertEquals(listOf(1, 3, 4, 5, 6, 7, 8, 9, 10), lineNumbers)
    }

    @ParameterizedTest
    @ValueSource(strings = ["xls", "xlsx"])
    fun `propagates a failure which the error handler rejects`(format: String) {

        assertFailsWith<IllegalStateException> {
            process(format, ::fillCellTypes, false, { false }) { _, _ ->
                throw IllegalStateException("Row cannot be imported")
            }
        }
    }

    @ParameterizedTest
    @ValueSource(strings = ["xls", "xlsx"])
    fun `imports only the first sheet by default`(format: String) {

        assertEquals(listOf(listOf("alpha-1"), listOf("alpha-2")), readSheets(format))
    }

    @ParameterizedTest
    @ValueSource(strings = ["xls", "xlsx"])
    fun `imports all sheets if requested`(format: String) {

        assertEquals(
            listOf(listOf("alpha-1"), listOf("alpha-2"), listOf("beta-1")),
            readSheets(format, importAllSheets = true)
        )
    }

    @ParameterizedTest
    @ValueSource(strings = ["xls", "xlsx"])
    fun `restarts the line numbers for every sheet`(format: String) {

        assertEquals(listOf(1, 2, 1), readNumberedRows(format, ::fillSheets, true).map { it.first })
    }

    @ParameterizedTest
    @ValueSource(strings = ["xls", "xlsx"])
    fun `runForSheets processes the sheets in the requested order`(format: String) {

        val rows = mutableListOf<Pair<Int, List<Any?>>>()

        sheetProcessorFor(format).runForSheets(
            collectingSheetProcessor("Beta", rows),
            collectingSheetProcessor("Alpha", rows)
        )

        // every sheet processor starts counting at one again
        assertEquals(listOf(1 to listOf("beta-1"), 1 to listOf("alpha-1"), 2 to listOf("alpha-2")), rows.toList())
    }

    /**
     * Writes the rows which cover the cell types and row shapes an import has to cope with.
     *
     * The row numbers are load bearing: the tests address the rows by the line number they arrive with,
     * and the gap in front of the last two rows is what pins that line numbers count processed rows
     * rather than spreadsheet rows.
     */
    private fun fillCellTypes(workbook: Workbook) {
        val sheet = workbook.createSheet("Data")

        val strings = sheet.createRow(0)
        strings.createCell(0).setCellValue("  padded  ")
        strings.createCell(1).setCellValue("plain")
        strings.createCell(2).setCellValue("umlauts äöüß")

        val numbers = sheet.createRow(1)
        numbers.createCell(0).setCellValue(42.0)
        numbers.createCell(1).setCellValue(3.5)
        numbers.createCell(2).setCellValue(-7.0)
        numbers.createCell(3).setCellValue(0.0)

        val dates = sheet.createRow(2)
        // a January date dodges every DST transition on earth, so that the conversion via
        // java.util.Date cannot shift it in any time zone
        dateCell(dates, 0, LocalDateTime.of(2026, 1, 2, 0, 0, 0), style(workbook, "yyyy-mm-dd"))
        dateCell(dates, 1, LocalDateTime.of(2026, 1, 2, 3, 4, 5), style(workbook, "yyyy-mm-dd hh:mm:ss"))

        val booleans = sheet.createRow(3)
        booleans.createCell(0).setCellValue(true)
        booleans.createCell(1).setCellValue(false)

        // the error formula sits first on purpose: as the last cell of the row it would be trimmed away
        val formulas = sheet.createRow(4)
        formulas.createCell(0).cellFormula = "1/0"
        formulas.createCell(1).cellFormula = "B2*2"
        formulas.createCell(2).cellFormula = "CONCATENATE(\"a\",\"b\")"
        formulas.createCell(3).cellFormula = "1=1"

        val gaps = sheet.createRow(5)
        gaps.createCell(0).setCellValue("start")
        // the cell in column B is missing entirely, the two cells behind "after gap" are empty
        gaps.createCell(2).setCellValue("after gap")
        gaps.createCell(3).setBlank()
        gaps.createCell(4).setCellValue("")

        sheet.createRow(6).createCell(0).setCellValue("   ")

        sheet.createRow(7)

        // leaves the spreadsheet rows 9 and 10 without a single cell
        sheet.createRow(10).createCell(0).setCellValue("after skipped rows")

        sheet.createRow(11).createCell(2).setCellValue("leading gap")
    }

    /**
     * Writes the two named sheets used to cover sheet selection.
     */
    private fun fillSheets(workbook: Workbook) {
        val alpha = workbook.createSheet("Alpha")
        alpha.createRow(0).createCell(0).setCellValue("alpha-1")
        alpha.createRow(1).createCell(0).setCellValue("alpha-2")
        val beta = workbook.createSheet("Beta")
        beta.createRow(0).createCell(0).setCellValue("beta-1")
    }

    private fun readCellTypes(format: String): List<List<Any?>> =
        readNumberedRows(format, ::fillCellTypes).map { it.second }

    private fun readSheets(format: String, importAllSheets: Boolean = false): List<List<Any?>> =
        readNumberedRows(format, ::fillSheets, importAllSheets).map { it.second }

    private fun readNumberedRows(
        format: String,
        fill: (Workbook) -> Unit,
        importAllSheets: Boolean = false
    ): List<Pair<Int, List<Any?>>> {
        val rows = mutableListOf<Pair<Int, List<Any?>>>()
        process(format, fill, importAllSheets, { false }) { lineNumber, row -> rows.add(lineNumber to row.asList()) }
        return rows
    }

    private fun process(
        format: String,
        fill: (Workbook) -> Unit,
        importAllSheets: Boolean,
        errorHandler: (Exception) -> Boolean,
        rowProcessor: (Int, Values) -> Unit
    ) {
        LineBasedProcessor.create("fixture.$format", workbookStream(format, fill), importAllSheets)
            .run({ lineNumber, row -> rowProcessor(lineNumber, row) }, errorHandler)
    }

    private fun sheetProcessorFor(format: String): XLSProcessor {
        val workbook = workbookStream(format, ::fillSheets)
        return if (format == "xlsx") XLSXProcessor(workbook, false) else XLSProcessor(workbook, false)
    }

    /**
     * Writes a workbook of the given format and hands it over as a stream, the way an upload would arrive.
     */
    private fun workbookStream(format: String, fill: (Workbook) -> Unit): ByteArrayInputStream {
        val bytes = ByteArrayOutputStream()
        val workbook: Workbook = if (format == "xlsx") XSSFWorkbook() else HSSFWorkbook()
        workbook.use {
            fill(it)
            evaluateFormulas(it)
            it.write(bytes)
        }
        return ByteArrayInputStream(bytes.toByteArray())
    }

    /**
     * Computes the cached results of every formula, as the streaming reader has no evaluator of its own
     * and reports a formula cell by its cached result alone.
     */
    private fun evaluateFormulas(workbook: Workbook) {
        val evaluator = workbook.creationHelper.createFormulaEvaluator()
        workbook.forEach { sheet: Sheet ->
            sheet.forEach { row: Row ->
                row.filter { it.cellType == CellType.FORMULA }.forEach { evaluator.evaluateFormulaCell(it) }
            }
        }
    }

    private fun dateCell(row: Row, column: Int, value: LocalDateTime, style: CellStyle) {
        val cell: Cell = row.createCell(column)
        cell.setCellValue(Date.from(value.atZone(ZoneId.systemDefault()).toInstant()))
        cell.cellStyle = style
    }

    private fun style(workbook: Workbook, format: String): CellStyle {
        val style = workbook.createCellStyle()
        style.dataFormat = workbook.creationHelper.createDataFormat().getFormat(format)
        return style
    }

    private fun collectingSheetProcessor(sheetName: String, target: MutableList<Pair<Int, List<Any?>>>) =
        SheetBasedRowProcessor(
            { lineNumber: Int, row: Values -> target.add(lineNumber to row.asList()) },
            Predicate { false },
            sheetName
        )

    private fun <T> withTimeZone(zone: String, block: () -> T): T {
        val previous = TimeZone.getDefault()
        TimeZone.setDefault(TimeZone.getTimeZone(zone))
        try {
            return block()
        } finally {
            TimeZone.setDefault(previous)
        }
    }

}
