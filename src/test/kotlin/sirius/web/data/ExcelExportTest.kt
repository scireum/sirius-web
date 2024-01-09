/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.data

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.extension.ExtendWith
import sirius.kernel.SiriusExtension
import sirius.kernel.commons.Files
import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream
import kotlin.test.assertEquals

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
            .run({ lineNum, row ->
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
            .run({ lineNum, row ->
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
            .run({ lineNum, row ->
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
            .run({ _, row ->
                assertEquals(data[currentData], row.asList())
                currentData++
            },
                { _ -> false })

        Files.delete(testFile)
    }

}
