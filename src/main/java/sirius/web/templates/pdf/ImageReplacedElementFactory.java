/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates.pdf;

import org.w3c.dom.Element;
import org.xhtmlrenderer.extend.ReplacedElement;
import org.xhtmlrenderer.extend.UserAgentCallback;
import org.xhtmlrenderer.layout.LayoutContext;
import org.xhtmlrenderer.pdf.ITextOutputDevice;
import org.xhtmlrenderer.pdf.ITextReplacedElementFactory;
import org.xhtmlrenderer.render.BlockBox;
import sirius.kernel.commons.Strings;
import sirius.kernel.di.std.PriorityParts;
import sirius.kernel.health.Exceptions;
import sirius.web.templates.pdf.handlers.PdfReplaceHandler;

import java.util.List;

/**
 * Used by the XHTMLRenderer (creating PDFs) to replace img elements by their referenced image.
 * <p>
 * Alongside http different URI protocols are supported. These are handled by classes extending
 * {@link PdfReplaceHandler}.
 */
public class ImageReplacedElementFactory extends ITextReplacedElementFactory {

    private static final String TAG_TYPE_IMG = "img";
    private static final String ATTR_SRC = "src";

    @PriorityParts(PdfReplaceHandler.class)
    private static List<PdfReplaceHandler> handlers;

    /**
     * Generates a new element factory for the given output
     *
     * @param outputDevice the output device to operate on
     */
    ImageReplacedElementFactory(ITextOutputDevice outputDevice) {
        super(outputDevice);
    }

    @Override
    public ReplacedElement createReplacedElement(LayoutContext c,
                                                 BlockBox box,
                                                 UserAgentCallback uac,
                                                 int cssWidth,
                                                 int cssHeight) {
        Element e = box.getElement();
        if (e == null) {
            return null;
        }

        String nodeName = e.getNodeName();
        if (!TAG_TYPE_IMG.equals(nodeName)) {
            return super.createReplacedElement(c, box, uac, cssWidth, cssHeight);
        }

        String src = e.getAttribute(ATTR_SRC);
        if (Strings.isEmpty(src)) {
            return super.createReplacedElement(c, box, uac, cssWidth, cssHeight);
        }

        try {
            String protocol = Strings.split(src, "://").getFirst();
            PdfReplaceHandler handler = findHandler(protocol);
            return new AsyncLoadedImageElement(handler, src, cssWidth, cssHeight);
        } catch (Exception ex) {
            Exceptions.handle(ex);
        }

        return super.createReplacedElement(c, box, uac, cssWidth, cssHeight);
    }

    private PdfReplaceHandler findHandler(String protocol) {
        return handlers.stream()
                       .filter(handler -> handler.accepts(protocol))
                       .findFirst()
                       .orElseThrow(() -> new UnsupportedOperationException(Strings.apply(
                               "No handler for protocol '%s' could be found",
                               protocol)));
    }
}
