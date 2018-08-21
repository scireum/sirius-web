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

class AssetsDispatcherSpec extends BaseSpecification {

    def "proper caching set for all kind of assets and cache-control URIs"(String uri, String header) {
        expect:
        URLConnection c = new URL("http://localhost:9999" + uri).openConnection()
        c.getHeaderField(HttpHeaderNames.CACHE_CONTROL.toString()) == header
        where:
        uri                               | header
        '/assets/test/test.css'           | 'public, max-age=3600'
        '/assets/test/test.txt'           | 'public, max-age=3600'
        '/assets/test/test.js'            | 'public, max-age=3600'
        '/assets/no-cache/test/test.css'  | 'no-cache, max-age=0'
        '/assets/no-cache/test/test.txt'  | 'no-cache, max-age=0'
        '/assets/no-cache/test/test.js'   | 'no-cache, max-age=0'
        '/assets/dynamic/X/test/test.css' | 'public, max-age=615168000'
        '/assets/dynamic/X/test/test.txt' | 'public, max-age=615168000'
        '/assets/dynamic/X/test/test.js'  | 'public, max-age=615168000'
    }

}
