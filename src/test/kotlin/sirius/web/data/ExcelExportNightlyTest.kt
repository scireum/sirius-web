/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.data

import org.junit.jupiter.api.Tag
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.assertDoesNotThrow
import org.junit.jupiter.api.extension.ExtendWith
import sirius.kernel.SiriusExtension
import sirius.kernel.Tags
import sirius.kernel.commons.Files
import sirius.kernel.di.std.Part
import sirius.kernel.health.Counter
import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream
import kotlin.test.assertEquals
import kotlin.test.assertTrue

@Tag(Tags.NIGHTLY)
@ExtendWith(SiriusExtension::class)
class ExcelExportNightlyTest {
    companion object {
        @Part
        @JvmStatic
        private val XLS_MAX_ROWS: Int = 0x10000

        @Part
        @JvmStatic
        private val XLSX_MAX_ROWS: Int = 0x100000
    }

    @Test
    fun `only allow 1 million rows in an xlsx excel sheet`() {

        val testFile = File.createTempFile("excel-output", ".xlsx")

        val export = ExcelExport.asStreamingXLSX()
        val sheetWithError = StringBuilder()
        export.withMaxRowsReachedHandler { sheetName -> sheetWithError.append(sheetName) }
        export.withMaxRowsReachedMessage("Max rows reached.")
        // overshoots the max num of rows a little for testing purposes
        for (i in 1..XLSX_MAX_ROWS + 100) {
            export.addArrayRow("A-$i")
        }
        export.writeToStream(FileOutputStream(testFile))

        assertEquals("Sheet0", sheetWithError.toString())

        val lineCounter = Counter()
        LineBasedProcessor.create(testFile.getName(), FileInputStream(testFile))
            .run({ lineNum, row ->
                lineCounter.inc()
                assertTrue { lineNum <= XLSX_MAX_ROWS }

                if (lineNum < XLSX_MAX_ROWS) {
                    assertEquals("A-$lineNum", row.at(0).asString())
                } else {
                    assertEquals("Max rows reached.", row.at(0).asString())
                    assertEquals(XLSX_MAX_ROWS, lineNum)
                }
            },
                { _ -> false })
        assertEquals(XLSX_MAX_ROWS, lineCounter.count.toInt())

        Files.delete(testFile)
    }

    @Test
    fun `only allow 65k rows in an xls excel sheet`() {

        val testFile = File.createTempFile("excel-output", ".xls")

        val export = ExcelExport.asXLS()
        val sheetWithError = StringBuilder()
        export.withMaxRowsReachedHandler { sheetName -> sheetWithError.append(sheetName) }
        export.withMaxRowsReachedMessage("Max rows reached.")
        // overshoot the max num of rows a little for testing purposes
        for (i in 1..XLS_MAX_ROWS + 100) {
            export.addArrayRow("A-$i")
        }
        export.writeToStream(FileOutputStream(testFile))

        assertEquals("Sheet0", sheetWithError.toString())

        val lineCounter = Counter()
        LineBasedProcessor.create(testFile.getName(), FileInputStream(testFile))
            .run({ lineNum, row ->
                lineCounter.inc()
                assertTrue { lineNum <= XLS_MAX_ROWS }
                if (lineNum < XLS_MAX_ROWS) {
                    assertEquals("A-$lineNum", row.at(0).asString())
                } else {
                    assertEquals("Max rows reached.", row.at(0).asString())
                    assertEquals(XLS_MAX_ROWS, lineNum)
                }
            },
                { _ -> false })
        assertEquals(XLS_MAX_ROWS, lineCounter.count.toInt())

        Files.delete(testFile)
    }

    @Test
    fun `use last line before row limit for data if no 'maxRowsReachedMessage' is given`() {

        val testFile = File.createTempFile("excel-output", ".xls")

        val export = ExcelExport.asXLS()
        // overshoot the max num of rows a little for testing purposes
        for (i in 1..XLS_MAX_ROWS + 100) {
            export.addArrayRow("A-$i")
        }
        export.writeToStream(FileOutputStream(testFile))

        val lineCounter = Counter()
        LineBasedProcessor.create(testFile.getName(), FileInputStream(testFile))
            .run({ lineNum, row ->
                lineCounter.inc()
                assertTrue { lineNum <= XLS_MAX_ROWS }
                assertEquals("A-$lineNum", row.at(0).asString())
            },
                { _ -> false })
        assertEquals(XLS_MAX_ROWS, lineCounter.count.toInt())

        Files.delete(testFile)
    }

    @Test
    fun `creation of  work sheet works correctly when max number of rows is reached in the current sheet`() {

        val testFile = File.createTempFile("excel-output", ".xls")

        val export = ExcelExport.asStreamingXLSX()
        // overshoot the max num of rows a little for testing purposes
        for (i in 1..XLSX_MAX_ROWS + 100) {
            export.addArrayRow("A-$i")
        }
        export.createSheet("Sheet1")
        export.addArrayRow("A row on Sheet1")

        assertDoesNotThrow {
            export.writeToStream(FileOutputStream(testFile))
        }
        Files.delete(testFile)
    }
}
