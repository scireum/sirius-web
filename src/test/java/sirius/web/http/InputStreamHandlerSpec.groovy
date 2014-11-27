/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.http

import io.netty.buffer.ByteBuf
import io.netty.buffer.Unpooled
import spock.lang.Specification
import spock.lang.Unroll

import java.nio.charset.Charset

class InputStreamHandlerSpec extends Specification {

    public static final Charset UTF_8 = Charset.forName("UTF-8")
    InputStreamHandler handler = new InputStreamHandler()

    static def emptyInput = ""
    static def shortInput = "Hallo Welt"
    static def longInput =  """Hallo Welt
Zeile 2
Zeile drei

Ende
"""
    static def emptyResult = []
    static def shortResult = [(shortInput)]
    static def longResult = ["Hallo Welt", "Zeile 2", "Zeile drei", "", "Ende"]

    static def BOM = "\uFEFF"

    @Unroll
    def "getContentsAsStrings() should return #expected when called for #input"(String input, List<String> expected) {
        given:
        ByteBuf b = Unpooled.copiedBuffer(input, UTF_8)
        handler.handle(b, true)
        when:
        def result = handler.getContentAsStrings()
        then:
        result == expected
        b.release()
        where:
        input << [emptyInput, shortInput, longInput]
        expected << [emptyResult, shortResult, longResult]
    }

    def "getContentsAsStrings() should remove leading BOM "() {
        given:
        ByteBuf b = Unpooled.copiedBuffer(BOM + shortInput, UTF_8)
        handler.handle(b, true)
        when:
        def result = handler.getContentAsStrings()
        then:
        result == shortResult
    }


}
