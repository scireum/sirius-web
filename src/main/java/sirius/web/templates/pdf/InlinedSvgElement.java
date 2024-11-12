/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates.pdf;

import com.lowagie.text.pdf.PdfContentByte;
import com.lowagie.text.pdf.PdfTemplate;
import org.apache.batik.transcoder.TranscoderInput;
import org.apache.batik.transcoder.print.PrintTranscoder;
import org.w3c.dom.Document;
import org.xhtmlrenderer.css.style.CalculatedStyle;
import org.xhtmlrenderer.layout.LayoutContext;
import org.xhtmlrenderer.pdf.ITextOutputDevice;
import org.xhtmlrenderer.pdf.ITextReplacedElement;
import org.xhtmlrenderer.render.BlockBox;
import org.xhtmlrenderer.render.PageBox;
import org.xhtmlrenderer.render.RenderingContext;

import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.print.PageFormat;
import java.awt.print.Paper;

/**
 * Represents an SVG element that is inlined into the PDF.
 * <p>
 * The SVG is rendered into a PDF template via Apache Batik and then placed into the PDF.
 *
 * @see <a href="https://stackoverflow.com/questions/37056791/svg-integration-in-pdf-using-flying-saucer">Stackoverflow</a>
 */
public class InlinedSvgElement implements ITextReplacedElement {

    private final Point location = new Point(0, 0);
    private final Document svg;
    private final int cssWidth;
    private final int cssHeight;

    protected InlinedSvgElement(Document svg, int cssWidth, int cssHeight) {
        this.svg = svg;
        this.cssWidth = cssWidth;
        this.cssHeight = cssHeight;
    }

    @Override
    public void paint(RenderingContext renderingContext, ITextOutputDevice outputDevice, BlockBox blockBox) {
        PdfContentByte contentByte = outputDevice.getWriter().getDirectContent();
        float width = cssWidth / outputDevice.getDotsPerPoint();
        float height = cssHeight / outputDevice.getDotsPerPoint();

        Paper paper = new Paper();
        paper.setSize(width, height);
        paper.setImageableArea(0, 0, width, height);

        PageFormat pageFormat = new PageFormat();
        pageFormat.setPaper(paper);

        PdfTemplate template = contentByte.createTemplate(width, height);
        Graphics2D graphics = template.createGraphics(width, height);
        PrintTranscoder printTranscoder = new PrintTranscoder();
        TranscoderInput transcoderInput = new TranscoderInput(svg);
        printTranscoder.transcode(transcoderInput, null);
        printTranscoder.print(graphics, pageFormat, 0);
        graphics.dispose();

        PageBox page = renderingContext.getPage();
        float x = (float) blockBox.getAbsX() + page.getMarginBorderPadding(renderingContext, CalculatedStyle.LEFT);
        float y = (float) (page.getBottom() - (blockBox.getAbsY() + cssHeight)) + page.getMarginBorderPadding(
                renderingContext,
                CalculatedStyle.BOTTOM);
        x /= outputDevice.getDotsPerPoint();
        y /= outputDevice.getDotsPerPoint();

        contentByte.addTemplate(template, x, y);
    }

    @Override
    public int getIntrinsicWidth() {
        return cssWidth;
    }

    @Override
    public int getIntrinsicHeight() {
        return cssHeight;
    }

    @Override
    public Point getLocation() {
        return location;
    }

    @Override
    public void setLocation(int x, int y) {
        location.setLocation(x, y);
    }

    @Override
    public void detach(LayoutContext layoutContext) {
        // nothing to do
    }

    @Override
    public boolean isRequiresInteractivePaint() {
        return false;
    }

    @Override
    public boolean hasBaseline() {
        return false;
    }

    @Override
    public int getBaseline() {
        return 0;
    }
}
