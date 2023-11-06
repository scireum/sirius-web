/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.dispatch

import io.netty.handler.codec.http.HttpHeaderNames
import sirius.kernel.BaseSpecification
import sirius.web.sass.Output

class AssetsDispatcherSpec extends BaseSpecification {

    def "proper caching set for all kind of assets and cache-control URIs"() {
        expect:
        URLConnection c = new URL("http://localhost:9999" + uri).openConnection()
        c.getHeaderField(HttpHeaderNames.CACHE_CONTROL.toString()) == header
        where:
        uri                               | header
        '/assets/test/test.css'           | 'public, max-age=21600'
        '/assets/test/test.txt'           | 'public, max-age=21600'
        '/assets/test/test.js'            | 'public, max-age=21600'
        '/assets/no-cache/test/test.css'  | 'no-cache, max-age=0'
        '/assets/no-cache/test/test.txt'  | 'no-cache, max-age=0'
        '/assets/no-cache/test/test.js'   | 'no-cache, max-age=0'
        '/assets/dynamic/X/test/test.css' | 'public, max-age=615168000'
        '/assets/dynamic/X/test/test.txt' | 'public, max-age=615168000'
        '/assets/dynamic/X/test/test.js'  | 'public, max-age=615168000'
    }

    def "proper custom proxy caching set for all cache-control URIs"() {
        expect:
        URLConnection c = new URL("http://localhost:9999" + uri).openConnection()
        c.getHeaderField("X-Custom-TTL") == header
        where:
        uri                               | header
        '/assets/test/test.css'           | '30d'
        '/assets/no-cache/test/test.css'  | null
        '/assets/dynamic/X/test/test.css' | null
    }

    def "Custom base64ResourceFunction works"() {
        when:
        SiriusSassGenerator gen = new SiriusSassGenerator()
        gen.importStylesheet("/assets/test_base64.scss")
        gen.compile()

        StringWriter writer = new StringWriter()
        gen.generate(new Output(writer, true))
        then:
        writer
                .toString() == "test { background: url(data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAADIAAAAyCAIAAACRXR/mAAAACXBIWXMAAAsTAAALEwEAmpwYAAAAB3RJTUUH4QsODw4S4KU/XgAAAB1pVFh0Q29tbWVudAAAAAAAQ3JlYXRlZCB3aXRoIEdJTVBkLmUHAAAAdElEQVRYw+3Q3Q1AMAAA4ar+qHhghq7SBQ3CPsQO5a1JS0wh+nA3wZdr5hBEfUlRZbBgwYIFCxYsWLBgwYIFCxYsWLBgwYIFCxYsWLBgfZSqDRRtWv1eC2vx2zFGV7S5pX2U+n3M2SWX1ZCNv6a+aHNL/bQvbxUXkThEKBQAAAAASUVORK5CYII=); }\n "
    }

}
