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

    def "ignores null inputs and does not write row"() {
        given:
        File testFile = File.createTempFile("excel-output", ".xlsx")
        when:
        ExcelExport export = ExcelExport.asStandardXLSX()
        export.addArrayRow(null)
        export.addListRow(null)
        export.addArrayRow("A-1", "B-1", "C-1")
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
        export.addArrayRow("A-1", "B-1", "C-1")
        export.addListRow(["A-2", "B-2", "C-2"] as ArrayList)
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
        export.addArrayRow("A-1", "B-1", "C-1")
        export.addListRow(["A-2", "B-2", "C-2"] as ArrayList)
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
        export.addListRow(data[0] as ArrayList)
        export.createSheet()
        export.addListRow(data[2] as ArrayList)
        export.setCurrentSheet(0)
        export.addListRow(data[1] as ArrayList)
        export.setCurrentSheet(1)
        export.addListRow(data[3] as ArrayList)
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

}
