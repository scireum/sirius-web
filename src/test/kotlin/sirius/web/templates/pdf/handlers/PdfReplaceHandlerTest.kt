/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates.pdf.handlers

import com.lowagie.text.ImgRaw
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.extension.ExtendWith
import org.xhtmlrenderer.pdf.ITextFSImage
import sirius.kernel.SiriusExtension
import kotlin.test.assertEquals

class PdfReplaceHandlerTest {

    @Test
    fun `scaling up the resource`() {
        val handler = ResourcePdfReplaceHandler()
        val rawImage = ITextFSImage(ImgRaw(50, 20, 1, 1, null))
        val scaledImage = handler.resizeImage(rawImage, 100, 100)
        assertEquals(100, scaledImage.width)
        assertEquals(40, scaledImage.height)
    }

    @Test
    fun `scaling up the width and scaling down the height`() {
        val handler = ResourcePdfReplaceHandler()
        val rawImage = ITextFSImage(ImgRaw(60, 50, 1, 1, null))
        val scaledImage = handler.resizeImage(rawImage, 100, 60)
        assertEquals(72, scaledImage.width)
        assertEquals(60, scaledImage.height)
    }

    @Test
    fun `scaling down the resource`() {
        val handler = ResourcePdfReplaceHandler()
        val rawImage = ITextFSImage(ImgRaw(150, 20, 1, 1, null))
        val scaledImage = handler.resizeImage(rawImage, 100, 100)
        assertEquals(100, scaledImage.width)
        assertEquals(13, scaledImage.height)
    }

    @Test
    fun `scaling down the height`() {
        val handler = ResourcePdfReplaceHandler()
        val rawImage = ITextFSImage(ImgRaw(50, 200, 1, 1, null))
        val scaledImage = handler.resizeImage(rawImage, 100, 100)
        assertEquals(25, scaledImage.width)
        assertEquals(100, scaledImage.height)
    }
}
