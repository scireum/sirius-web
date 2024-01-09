/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.data

import org.junit.jupiter.api.Tag
import sirius.kernel.BaseSpecification
import sirius.kernel.Tags
import sirius.kernel.commons.Files
import sirius.kernel.health.Counter

@Tag(Tags.NIGHTLY)
class ExcelExportNightlySpec extends BaseSpecification {

    private static final int XLS_MAX_ROWS = 0x10000
    private static final int XLSX_MAX_ROWS = 0x100000

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
        export.addArrayRow("A-" + i)
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
        export.addArrayRow("A-" + i)
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

    def "use last line before row limit for data if no 'maxRowsReachedMessage' is given"() {
        given:
        File testFile = File.createTempFile("excel-output", ".xls")
        when:
        ExcelExport export = ExcelExport.asXLS()
        // overshoot the max num of rows a little for testing purposes
        for (int i = 1; i <= XLS_MAX_ROWS + 100; i++) {
        export.addArrayRow("A-" + i)
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

    def "creation of new work sheet works correctly when max number of rows is reached in the current sheet"() {
        given:
        File testFile = File.createTempFile("excel-output", ".xls")
        when:
        ExcelExport export = ExcelExport.asStreamingXLSX()
        // overshoot the max num of rows a little for testing purposes
        for (int i = 1; i <= XLSX_MAX_ROWS + 100; i++) {
        export.addArrayRow("A-" + i)
    }
        export.createSheet("Sheet1")
        export.addArrayRow("A row on Sheet1")
        export.writeToStream(new FileOutputStream(testFile))
        then:
        noExceptionThrown()
        cleanup:
        Files.delete(testFile)
    }
}
