/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.http

import com.alibaba.fastjson.JSON
import com.google.common.io.ByteStreams
import org.apache.commons.io.Charsets
import sirius.kernel.BaseSpecification
import sirius.kernel.Scope

/**
 * Simulates "real" uploads through netty and sirius.
 */
@Scope(Scope.SCOPE_NIGHTLY)
class UploadSpec extends BaseSpecification {

    String lineFeed = "\r\n"

    def upload(String uri, File file) {
        HttpURLConnection connection = new URL("http://localhost:9999" + uri).openConnection()

        InputStream inputStream = new FileInputStream(file)
        connection.setDoOutput(true)
        connection.setRequestMethod("POST")
        OutputStream outputStream = connection.getOutputStream()
        ByteStreams.copy(inputStream, outputStream)
        inputStream.close()
        outputStream.flush()

        return new String(ByteStreams.toByteArray(connection.getInputStream()), Charsets.UTF_8)
    }

    def "Uploads a file to /upload-test"() {
        given:
        File file = new File("src/test/resources/test.png")
        when:
        def response = upload("/upload-test", file)
        and:
        def json = JSON.parseObject(response)
        then:
        json.get("success") == true
        and:
        json.get("size") == file.length()
    }

    def "Uploads a gzip-file to /upload-gzip"() {
        given:
        File file = new File("src/test/resources/test.csv.gz")
        when:
        def response = upload("/upload-gzip", file)
        and:
        def json = JSON.parseObject(response)
        then:
        json.get("success") == true
        and:
        json.get("lines") == 3
    }
}
