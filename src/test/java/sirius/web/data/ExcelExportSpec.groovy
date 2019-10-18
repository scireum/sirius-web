/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.data

import sirius.kernel.BaseSpecification
import sirius.kernel.commons.Files
import sirius.kernel.health.Counter
import sirius.kernel.nls.NLS

class ExcelExportSpec extends BaseSpecification {

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

    def "only allow 1 million in a excel sheet rows"() {
        given:
        File testFile = File.createTempFile("excel-output", ".xlsx")
        when:
        ExcelExport export = ExcelExport.asStreamingXLSX()
        StringBuilder sheetWithError = new StringBuilder()
        export.setMaxRowsReachedHandler({ sheetName -> sheetWithError.append(sheetName) })
        for (int i = 1; i <= ExcelExport.MAX_NUM_ROWS; i++) {
            export.addRow("A-" + i)
        }
        export.addRow("A-" + (ExcelExport.MAX_NUM_ROWS + 1))
        export.writeToStream(new FileOutputStream(testFile))
        then:
        sheetWithError.toString() == "Sheet0"
        and:
        Counter lineCounter = new Counter()
        LineBasedProcessor.create(testFile.getName(), new FileInputStream(testFile))
                          .run({
                                   lineNum, row ->
                                       lineCounter.inc()
                                       assert lineNum <= ExcelExport.MAX_NUM_ROWS + 1
                                       if (lineNum <= (ExcelExport.MAX_NUM_ROWS)) {
                                           assert row.at(0).asString() == "A-" + lineNum
                                       } else {
                                           assert row.at(0) == NLS.get("ExcelExport.maxRowsReached")
                                           assert lineNum == ExcelExport.MAX_NUM_ROWS + 1
                                       }
                               },
                               { e -> false })
        lineCounter.getCount() == ExcelExport.MAX_NUM_ROWS + 1
        cleanup:
        Files.delete(testFile)
    }
}
