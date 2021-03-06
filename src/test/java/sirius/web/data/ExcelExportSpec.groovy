/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.data

import sirius.kernel.BaseSpecification
import sirius.kernel.Scope
import sirius.kernel.commons.Files
import sirius.kernel.health.Counter

class ExcelExportSpec extends BaseSpecification {

    private static final int XLS_MAX_ROWS = 0x10000
    private static final int XLSX_MAX_ROWS = 0x100000

    def "ignores null inputs and does not write row"() {
        given:
        File testFile = File.createTempFile("excel-output", ".xlsx")
        when:
        ExcelExport export = ExcelExport.asStandardXLSX()
        export.addRow(null)
        export.addRowAsList(null)
        export.addRow("A-1", "B-1", "C-1")
        export.writeToStream(new FileOutputStream(testFile))
        then:
        def expectLineNum = 1
        def expectedRow = [["A-1", "B-1", "C-1"]]
        LineBasedProcessor.create(testFile.getName(), new FileInputStream(testFile))
                          .run({
                                   lineNum, row ->
                                       assert lineNum == expectLineNum++
                                       assert row.asList() == expectedRow[lineNum - 1]
                               },
                               { e -> false })
        cleanup:
        Files.delete(testFile)
    }

    def "create simple excel sheet"() {
        given:
        File testFile = File.createTempFile("excel-output", ".xlsx")
        when:
        ExcelExport export = ExcelExport.asStandardXLSX()
        export.addRow("A-1", "B-1", "C-1")
        export.addRowAsList(["A-2", "B-2", "C-2"] as ArrayList)
        export.writeToStream(new FileOutputStream(testFile))
        then:
        def expectLineNum = 1
        def expectedRow = [["A-1", "B-1", "C-1"], ["A-2", "B-2", "C-2"]]
        LineBasedProcessor.create(testFile.getName(), new FileInputStream(testFile))
                          .run({
                                   lineNum, row ->
                                       assert lineNum == expectLineNum++
                                       assert row.asList() == expectedRow[lineNum - 1]
                               },
                               { e -> false })
        cleanup:
        Files.delete(testFile)
    }

    def "create simple streaming excel sheet"() {
        given:
        File testFile = File.createTempFile("excel-output", ".xlsx")
        when:
        ExcelExport export = ExcelExport.asStreamingXLSX()
        export.addRow("A-1", "B-1", "C-1")
        export.addRowAsList(["A-2", "B-2", "C-2"] as ArrayList)
        export.writeToStream(new FileOutputStream(testFile))
        then:
        def expectLineNum = 1
        def expectedRow = [["A-1", "B-1", "C-1"], ["A-2", "B-2", "C-2"]]
        LineBasedProcessor.create(testFile.getName(), new FileInputStream(testFile))
                          .run({
                                   lineNum, row ->
                                       assert lineNum == expectLineNum++
                                       assert row.asList() == expectedRow[lineNum - 1]
                               },
                               { e -> false })
        cleanup:
        Files.delete(testFile)
    }

    def "create excel sheet with multiple sheets"() {
        given:
        File testFile = File.createTempFile("excel-output", ".xlsx")
        def data = [["S1-A-1", "S1-B-1", "S1-C-1"], ["S1-A-2", "S1-B-2", "S1-C-2"], ["S2-A-1", "S2-B-1", "S2-C-1"], ["S2-A-2", "S2-B-2", "S2-C-2"]]
        when:
        ExcelExport export = ExcelExport.asStreamingXLSX(false)
        export.createSheet()
        export.addRowAsList(data[0] as ArrayList)
        export.createSheet()
        export.addRowAsList(data[2] as ArrayList)
        export.setCurrentSheet(0)
        export.addRowAsList(data[1] as ArrayList)
        export.setCurrentSheet(1)
        export.addRowAsList(data[3] as ArrayList)
        export.writeToStream(new FileOutputStream(testFile))
        then:
        def currentData = 0
        XLSXProcessor.create(testFile.getName(), new FileInputStream(testFile), true)
                     .run({
                              lineNum, row ->
                                  assert row.asList() == data[currentData]
                                  currentData++
                          },
                          { e -> false })
        cleanup:
        Files.delete(testFile)
    }

    @Scope(Scope.SCOPE_NIGHTLY)
    def "only allow 1 million rows in an xlsx excel sheet"() {
        given:
        File testFile = File.createTempFile("excel-output", ".xlsx")
        when:
        ExcelExport export = ExcelExport.asStreamingXLSX()
        StringBuilder sheetWithError = new StringBuilder()
        export.withMaxRowsReachedHandler({ sheetName -> sheetWithError.append(sheetName) })
        export.withMaxRowsReachedMessage("Max rows reached.")
        // overshoot the max num of rows a little for testing purposes
        for (int i = 1; i <= XLSX_MAX_ROWS + 100; i++) {
            export.addRow("A-" + i)
        }
        export.writeToStream(new FileOutputStream(testFile))
        then:
        sheetWithError.toString() == "Sheet0"
        and:
        Counter lineCounter = new Counter()
        LineBasedProcessor.create(testFile.getName(), new FileInputStream(testFile))
                          .run({
                                   lineNum, row ->
                                       lineCounter.inc()
                                       assert lineNum <= XLSX_MAX_ROWS
                                       if (lineNum < XLSX_MAX_ROWS) {
                                           assert row.at(0).asString() == "A-" + lineNum
                                       } else {
                                           assert row.at(0) == "Max rows reached."
                                           assert lineNum == XLSX_MAX_ROWS
                                       }
                               },
                               { e -> false })
        lineCounter.getCount() == XLSX_MAX_ROWS
        cleanup:
        Files.delete(testFile)
    }

    @Scope(Scope.SCOPE_NIGHTLY)
    def "only allow 65k rows in an xls excel sheet"() {
        given:
        File testFile = File.createTempFile("excel-output", ".xls")
        when:
        ExcelExport export = ExcelExport.asXLS()
        StringBuilder sheetWithError = new StringBuilder()
        export.withMaxRowsReachedHandler({ sheetName -> sheetWithError.append(sheetName) })
        export.withMaxRowsReachedMessage("Max rows reached.")
        // overshoot the max num of rows a little for testing purposes
        for (int i = 1; i <= XLS_MAX_ROWS + 100; i++) {
            export.addRow("A-" + i)
        }
        export.writeToStream(new FileOutputStream(testFile))
        then:
        sheetWithError.toString() == "Sheet0"
        and:
        Counter lineCounter = new Counter()
        LineBasedProcessor.create(testFile.getName(), new FileInputStream(testFile))
                          .run({
                                   lineNum, row ->
                                       lineCounter.inc()
                                       assert lineNum <= XLS_MAX_ROWS
                                       if (lineNum < XLS_MAX_ROWS) {
                                           assert row.at(0).asString() == "A-" + lineNum
                                       } else {
                                           assert row.at(0) == "Max rows reached."
                                           assert lineNum == XLS_MAX_ROWS
                                       }
                               },
                               { e -> false })
        lineCounter.getCount() == XLS_MAX_ROWS
        cleanup:
        Files.delete(testFile)
    }

    @Scope(Scope.SCOPE_NIGHTLY)
    def "use last line before row limit for data if no 'maxRowsReachedMessage' is given"() {
        given:
        File testFile = File.createTempFile("excel-output", ".xls")
        when:
        ExcelExport export = ExcelExport.asXLS()
        // overshoot the max num of rows a little for testing purposes
        for (int i = 1; i <= XLS_MAX_ROWS + 100; i++) {
            export.addRow("A-" + i)
        }
        export.writeToStream(new FileOutputStream(testFile))
        then:
        Counter lineCounter = new Counter()
        LineBasedProcessor.create(testFile.getName(), new FileInputStream(testFile))
                          .run({
                                   lineNum, row ->
                                       lineCounter.inc()
                                       assert lineNum <= XLS_MAX_ROWS
                                       assert row.at(0).asString() == "A-" + lineNum
                               },
                               { e -> false })
        lineCounter.getCount() == XLS_MAX_ROWS
        cleanup:
        Files.delete(testFile)
    }

    @Scope(Scope.SCOPE_NIGHTLY)
    def "creation of new work sheet works correctly when max number of rows is reached in the current sheet"() {
        given:
        File testFile = File.createTempFile("excel-output", ".xls")
        when:
        ExcelExport export = ExcelExport.asStreamingXLSX()
        // overshoot the max num of rows a little for testing purposes
        for (int i = 1; i <= XLSX_MAX_ROWS + 100; i++) {
            export.addRow("A-" + i)
        }
        export.createSheet("Sheet1")
        export.addRow("A row on Sheet1")
        export.writeToStream(new FileOutputStream(testFile))
        then:
        noExceptionThrown()
        cleanup:
        Files.delete(testFile)
    }
}
