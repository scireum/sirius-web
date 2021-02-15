/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates.pdf;

import org.w3c.dom.Element;
import org.xhtmlrenderer.extend.FSImage;
import org.xhtmlrenderer.extend.ReplacedElement;
import org.xhtmlrenderer.extend.UserAgentCallback;
import org.xhtmlrenderer.layout.LayoutContext;
import org.xhtmlrenderer.pdf.ITextImageElement;
import org.xhtmlrenderer.pdf.ITextOutputDevice;
import org.xhtmlrenderer.pdf.ITextReplacedElementFactory;
import org.xhtmlrenderer.render.BlockBox;
import sirius.kernel.commons.Strings;
import sirius.kernel.di.std.PriorityParts;
import sirius.kernel.health.Exceptions;
import sirius.web.templates.pdf.handlers.PdfReplaceHandler;

import java.util.List;
import java.util.Optional;

/**
 * Used by the XHTMLRenderer (creating PDFs) to replace img elements by their references image.
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
            Optional<FSImage> image = resolveUri(src, uac, cssWidth, cssHeight);

            if (image.isPresent()) {
                return new ITextImageElement(image.get());
            }
        } catch (Exception ex) {
            Exceptions.handle(ex);
        }

        return super.createReplacedElement(c, box, uac, cssWidth, cssHeight);
    }

    private Optional<FSImage> resolveUri(String uri, UserAgentCallback userAgentCallback, int cssWidth, int cssHeight) {
        String protocol = Strings.split(uri, "://").getFirst();
        PdfReplaceHandler handler = findHandler(protocol);

        try {
            FSImage image = handler.resolveUri(uri, userAgentCallback, cssWidth, cssHeight);

            if (image != null) {
                return Optional.of(image);
            }
        } catch (Exception e) {
            Exceptions.handle(e);
        }

        return Optional.empty();
    }

    private PdfReplaceHandler findHandler(String protocol) {
        for (PdfReplaceHandler handler : handlers) {
            if (handler.accepts(protocol)) {
                return handler;
            }
        }

        throw new UnsupportedOperationException(Strings.apply("No handler for protocol '%s' could be found", protocol));
    }
}
