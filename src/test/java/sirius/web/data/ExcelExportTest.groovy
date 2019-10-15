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
}
