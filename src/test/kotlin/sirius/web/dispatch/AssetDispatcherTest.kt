/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.dispatch

import io.netty.handler.codec.http.HttpHeaderNames
import org.junit.jupiter.api.extension.ExtendWith
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.CsvSource
import sirius.kernel.SiriusExtension
import sirius.web.sass.Output
import java.io.StringWriter
import java.net.URL
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals


/**
 * Tests the [AssetsDispatcher] class.
 */
@ExtendWith(SiriusExtension::class)
class AssetsDispatcherTest {
    @CsvSource(
            delimiter = '|', useHeadersInDisplayName = true, textBlock = """
         uri                            | header
        /assets/test/test.css           | public, max-age=21600
        /assets/test/test.txt           | public, max-age=21600
        /assets/test/test.js            | public, max-age=21600
        /assets/no-cache/test/test.css  | no-cache, max-age=0
        /assets/no-cache/test/test.txt  | no-cache, max-age=0
        /assets/no-cache/test/test.js   | no-cache, max-age=0
        /assets/dynamic/X/test/test.css | public, max-age=615168000
        /assets/dynamic/X/test/test.txt | public, max-age=615168000
        /assets/dynamic/X/test/test.js  | public, max-age=615168000"""
    )
    @ParameterizedTest
    fun `proper caching set for all kind of assets and cache-control URIs`(uri: String, header: String) {
        val connection = URL("http://localhost:9999" + uri).openConnection()
        assertEquals(header, connection.getHeaderField(HttpHeaderNames.CACHE_CONTROL.toString()))
    }

    @Test
    fun `Custom base64ResourceFunction works`() {
        val generator = SiriusSassGenerator()
        generator.importStylesheet("/assets/test_base64.scss")
        generator.compile()
        val writer = StringWriter()
        generator.generate(Output(writer, true))
        assertEquals(
                "test { background: url(data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAADIAAAAyCAIAAACRXR/mAAAACXBIWXMAAAsTAAALEwEAmpwYAAAAB3RJTUUH4QsODw4S4KU/XgAAAB1pVFh0Q29tbWVudAAAAAAAQ3JlYXRlZCB3aXRoIEdJTVBkLmUHAAAAdElEQVRYw+3Q3Q1AMAAA4ar+qHhghq7SBQ3CPsQO5a1JS0wh+nA3wZdr5hBEfUlRZbBgwYIFCxYsWLBgwYIFCxYsWLBgwYIFCxYsWLBgfZSqDRRtWv1eC2vx2zFGV7S5pX2U+n3M2SWX1ZCNv6a+aHNL/bQvbxUXkThEKBQAAAAASUVORK5CYII=); }\n ",
                writer.toString()
        )
    }

}
