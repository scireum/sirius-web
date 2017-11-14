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

/**
 * Simulates "real" uploads through netty and sirius.
 */
class UploadSpec extends BaseSpecification {

    String lineFeed = "\r\n";

    def upload(String uri, File file) {
        HttpURLConnection connection = new URL("http://localhost:9999" + uri).openConnection()

        InputStream inputStream = new FileInputStream(file)

        connection.setDoOutput(true)
        connection.setUseCaches(false)

        connection.setRequestMethod("POST")

        connection.setRequestProperty("Connection", "keep-alive");
        connection.setRequestProperty("Content-Type", "application/octet-stream");

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
}
