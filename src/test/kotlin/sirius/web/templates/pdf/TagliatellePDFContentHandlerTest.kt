/*
 * Made with all the love in the world
 * by scireum in Stuttgart, Germany
 *
 * Copyright by scireum GmbH
 * https://www.scireum.de - info@scireum.de
 */

package sirius.web.templates.pdf

import com.sun.net.httpserver.HttpServer
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.extension.ExtendWith
import org.openpdf.text.pdf.PRStream
import org.openpdf.text.pdf.PdfDictionary
import org.openpdf.text.pdf.PdfName
import org.openpdf.text.pdf.PdfReader
import org.openpdf.text.pdf.parser.PdfTextExtractor
import sirius.kernel.SiriusExtension
import sirius.kernel.di.std.Part
import sirius.web.templates.Templates
import java.io.ByteArrayOutputStream
import java.net.InetSocketAddress
import java.util.concurrent.atomic.AtomicInteger
import kotlin.test.assertEquals
import kotlin.test.assertTrue

/**
 * Renders small `.pdf.pasta` templates through [TagliatellePDFContentHandler] and pins the properties of the
 * resulting PDF which templates rely on.
 *
 * These tests intentionally exercise flying-saucer and OpenPDF end to end, so that a dependency bump which changes
 * the layout or the output of generated PDFs fails here instead of going unnoticed.
 */
@ExtendWith(SiriusExtension::class)
class TagliatellePDFContentHandlerTest {

    @Test
    fun `renders text in the registered Open Sans fonts`() {
        val pdf = render("<p>Regular <b>Bold</b> <i>Italic</i></p>")

        // the text extractor guesses spaces between differently styled runs, hence these are ignored
        assertEquals("RegularBoldItalic", pdf.text(1).replace(Regex("\\s"), ""))
        assertEquals(
            listOf("OpenSans", "OpenSans-Bold", "OpenSans-Italic"),
            pdf.fonts(1).map { it.substringAfter('+') }.sorted()
        )
        assertTrue(pdf.fonts(1).all { it.matches(Regex("[A-Z]{6}\\+.*")) }, "fonts must be subsets")
        assertTrue(pdf.fontDictionaries(1).all { isEmbedded(it) }, "fonts must be embedded")
    }

    @Test
    fun `wraps text in a narrow table cell and keeps line breaks`() {
        val pdf = render(
            """
            <table style="width: 30mm; border-collapse: collapse;">
                <tr><td style="padding: 0;">Kreuzschlitz-Schraubendreher mit Magnetspitze<br/>Art. 4711</td></tr>
            </table>
            """
        )

        assertEquals(listOf("Kreuzschlitz-", "Schraubendreher", "mit Magnetspitze", "Art. 4711"), pdf.lines(1))
    }

    @Test
    fun `breaks overflowing content onto further pages`() {
        val pdf = render((1..120).joinToString("") { "<p>Line $it</p>" })

        assertEquals(3, pdf.numberOfPages)
        assertEquals("Line 1", pdf.lines(1).first())
        assertEquals("Line 120", pdf.lines(pdf.numberOfPages).last())
    }

    @Test
    fun `draws inline svg elements as vector graphics`() {
        val pdf = render(
            """
            <svg xmlns="http://www.w3.org/2000/svg" width="32" height="32" viewBox="0 0 32 32">
                <rect x="4" y="4" width="24" height="24" fill="#ff0000"/>
            </svg>
            """
        )

        assertDrawnAsVector(pdf)
    }

    @Test
    fun `embeds images loaded via the resource protocol`() {
        val pdf = render("""<img src="resource://assets/test.png" style="width: 50px; height: 50px;"/>""")

        assertEquals(listOf(50 to 50), pdf.images(1))
    }

    @Test
    fun `draws svg images loaded via the resource protocol as vector graphics`() {
        val pdf = render("""<img src="resource://assets/test.svg" style="width: 32px; height: 32px;"/>""")

        assertDrawnAsVector(pdf)
    }

    @Test
    fun `sizes svg images by their absolute width and height before their view box`() {
        // 0.5in x 36pt is square, unlike the view box (3:1) and the default size (2:1)
        val pdf = render("""<img src="resource://assets/test-units.svg" style="width: 96px; height: 96px;"/>""")

        assertDrawnAsVector(pdf)
        assertEquals(listOf(72f to 72f), pdf.formSizes(1), "96 x 96 px")
    }

    @Test
    fun `sizes svg images without width and height by their view box`() {
        // the view box (3:1) differs from the default size (2:1) and from 1 x 1
        val pdf = render("""<img src="resource://assets/test-viewbox.svg" style="width: 96px; height: 96px;"/>""")

        assertDrawnAsVector(pdf)
        assertEquals(listOf(72f to 24f), pdf.formSizes(1), "96 x 32 px")
    }

    @Test
    fun `leaves out svg images which cannot be drawn instead of failing the document`() {
        val pdf = render(
            """
            <p>Before</p>
            <img src="resource://assets/test-external-reference.svg" style="width: 32px; height: 32px;"/>
            <p>After</p>
            """
        )

        assertEquals(listOf("Before", "After"), pdf.lines(1))
        // the referenced test.png exists next to the svg, so it must have been refused rather than not found
        assertEquals(emptyList(), pdf.images(1), "the external reference must not be loaded")
        assertEquals(emptyList(), pdf.formSizes(1), "the svg must be left out entirely")
    }

    @Test
    fun `draws svg images loaded via http as vector graphics`() {
        val (pdf, requests) = serving("/logo.svg", "image/svg+xml", "/assets/test.svg") { url ->
            render("""<img src="$url" style="width: 32px; height: 32px;"/>""")
        }

        assertDrawnAsVector(pdf)
        assertEquals(1, requests, "the svg must be loaded on the first attempt")
    }

    @Test
    fun `draws svg images loaded via http without svg extension as vector graphics`() {
        // without the extension, only the last attempt via flying-saucer's user agent recognises the svg
        val (pdf, _) = serving("/logo", "image/svg+xml", "/assets/test.svg") { url ->
            render("""<img src="$url" style="width: 32px; height: 32px;"/>""")
        }

        assertDrawnAsVector(pdf)
    }

    @Test
    fun `loads nothing an svg image references from outside`() {
        // the external DTD, entities and image are relative, so they would hit the server if they were resolved
        val (_, requests) = serving("/images/hostile.svg", "image/svg+xml", "/assets/test-hostile.svg") { url ->
            render("""<img src="$url" style="width: 32px; height: 32px;"/>""")
        }

        assertEquals(1, requests, "only the svg itself must be requested")
    }

    /**
     * Renders the given body markup as A4 PDF with 20mm page margins, using Open Sans at 10pt.
     */
    private fun render(body: String): PdfReader {
        val template = """
            <html>
            <head>
                <style>
                    @@page { size: A4; margin: 20mm; }
                    body { font-family: 'Open Sans'; font-size: 10pt; margin: 0; }
                    p { margin: 0; }
                </style>
            </head>
            <body>$body</body>
            </html>
            """.trimIndent()
        val out = ByteArrayOutputStream()
        templates.generator().direct(template, TagliatellePDFContentHandler.PDF_PASTA).generateTo(out)
        return PdfReader(out.toByteArray())
    }

    /**
     * Serves the given classpath resource via a throwaway HTTP server while running the action.
     *
     * Returns the result of the action along with the number of requests the server received for any path.
     *
     * The served path must not contain `/assets/`: [sirius.web.templates.pdf.handlers.ResourcePdfReplaceHandler]
     * rewrites such URLs to the resource protocol, so they would never be loaded via HTTP.
     */
    private fun <T> serving(path: String, contentType: String, resource: String, action: (String) -> T): Pair<T, Int> {
        val content = javaClass.getResourceAsStream(resource)!!.use { it.readBytes() }
        val server = HttpServer.create(InetSocketAddress(LOOPBACK, 0), 0)
        val requests = AtomicInteger()
        server.createContext("/") { exchange ->
            requests.incrementAndGet()
            exchange.responseHeaders.add("Content-Type", contentType)
            exchange.sendResponseHeaders(200, content.size.toLong())
            exchange.responseBody.use { it.write(content) }
        }
        server.start()
        try {
            return action("http://$LOOPBACK:${server.address.port}$path") to requests.get()
        } finally {
            server.stop(0)
        }
    }

    private fun PdfReader.text(page: Int) = PdfTextExtractor(this).getTextFromPage(page).trim()

    private fun PdfReader.lines(page: Int) = text(page).lines().map { it.trim() }.filter { it.isNotEmpty() }

    private fun PdfReader.resources(page: Int) = getPageN(page).getAsDict(PdfName.RESOURCES)

    private fun PdfReader.fontDictionaries(page: Int): List<PdfDictionary> {
        val fonts = resources(page).getAsDict(PdfName.FONT) ?: return emptyList()
        return fonts.keys.map { fonts.getAsDict(it) }
    }

    private fun PdfReader.fonts(page: Int) =
        fontDictionaries(page).map { it.getAsName(PdfName.BASEFONT).toString().removePrefix("/") }

    /**
     * Determines whether the font program is embedded, i.e. whether the font descriptor of the font (or of its
     * descendant font for composite fonts) carries a TrueType font file.
     */
    private fun isEmbedded(font: PdfDictionary): Boolean {
        val descendant = font.getAsArray(PdfName.DESCENDANTFONTS)?.getAsDict(0) ?: font
        return descendant.getAsDict(PdfName.FONTDESCRIPTOR)?.contains(PdfName.FONTFILE2) == true
    }

    /**
     * Collects the XObjects used on the given page, including the ones nested within form XObjects.
     */
    private fun PdfReader.xObjects(page: Int) = nestedXObjects(resources(page))

    private fun nestedXObjects(resources: PdfDictionary?): List<PRStream> {
        val xObjects = resources?.getAsDict(PdfName.XOBJECT) ?: return emptyList()
        return xObjects.keys.map { PdfReader.getPdfObject(xObjects.get(it)) as PRStream }
            .flatMap { listOf(it) + nestedXObjects(it.getAsDict(PdfName.RESOURCES)) }
    }

    /**
     * Lists the raster images used on the given page as their pixel dimensions, wherever they are nested.
     */
    private fun PdfReader.images(page: Int): List<Pair<Int, Int>> =
        xObjects(page).filter { it.getAsName(PdfName.SUBTYPE) == PdfName.IMAGE }
            .map { it.getAsNumber(PdfName.WIDTH).intValue() to it.getAsNumber(PdfName.HEIGHT).intValue() }

    /**
     * Counts the paths filled or stroked within the form XObjects of the given page, i.e. the drawn vector shapes.
     */
    private fun PdfReader.paintedPaths(page: Int) =
        xObjects(page).filter { it.getAsName(PdfName.SUBTYPE) == PdfName.FORM }
            .sumOf { PATH_PAINTING_OPERATOR.findAll(String(PdfReader.getStreamBytes(it), Charsets.ISO_8859_1)).count() }

    /**
     * Lists the bounding boxes of the form XObjects used on the given page as their width and height in points.
     */
    private fun PdfReader.formSizes(page: Int): List<Pair<Float, Float>> =
        xObjects(page).filter { it.getAsName(PdfName.SUBTYPE) == PdfName.FORM }.map {
            val box = it.getAsArray(PdfName.BBOX)
            box.getAsNumber(2).floatValue() - box.getAsNumber(0).floatValue() to
                    box.getAsNumber(3).floatValue() - box.getAsNumber(1).floatValue()
        }

    /**
     * Asserts that the first page shows vector shapes and contains no raster image at all, not even one hidden in a
     * form XObject.
     */
    private fun assertDrawnAsVector(pdf: PdfReader) {
        assertEquals(emptyList(), pdf.images(1), "the svg must not be rasterized")
        assertTrue(pdf.paintedPaths(1) > 0, "the svg must be drawn as vector paths")
    }

    companion object {
        private const val LOOPBACK = "127.0.0.1"

        /**
         * Matches the content stream operators which fill and/or stroke the current path.
         */
        private val PATH_PAINTING_OPERATOR = Regex("""(?<=\s)(f\*?|F|B\*?|b\*?|S|s)(?=\s)""")

        @Part
        @JvmStatic
        private lateinit var templates: Templates
    }
}
