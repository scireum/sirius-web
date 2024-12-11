/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates.pdf;

import org.w3c.dom.Document;
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

import javax.annotation.Nonnull;
import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import java.util.List;
import java.util.Optional;

/**
 * Used by the XHTMLRenderer (creating PDFs) to replace {@code img} elements by their referenced image and to render
 * inline {@code svg} elements.
 * <p>
 * Alongside http different URI protocols are supported. These are handled by classes extending
 * {@link PdfReplaceHandler}.
 */
public class ImageReplacedElementFactory extends ITextReplacedElementFactory {

    private static final String TAG_TYPE_IMG = "img";
    private static final String TAG_TYPE_SVG = "svg";

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
    public ReplacedElement createReplacedElement(LayoutContext layoutContext,
                                                 BlockBox box,
                                                 UserAgentCallback userAgentCallback,
                                                 int cssWidth,
                                                 int cssHeight) {
        Element element = box.getElement();
        if (element == null) {
            return null;
        }

        return this.tryCreateReplacedImageElement(element, userAgentCallback, cssWidth, cssHeight)
                   .or(() -> this.tryCreateReplacedSvgElement(element, cssWidth, cssHeight))
                   .orElseGet(() -> super.createReplacedElement(layoutContext,
                                                                box,
                                                                userAgentCallback,
                                                                cssWidth,
                                                                cssHeight));
    }

    private Optional<ReplacedElement> tryCreateReplacedSvgElement(@Nonnull Element element,
                                                                  int cssWidth,
                                                                  int cssHeight) {
        String nodeName = element.getNodeName();
        if (!TAG_TYPE_SVG.equals(nodeName)) {
            return Optional.empty();
        }

        try {
            DocumentBuilderFactory documentBuilderFactory = DocumentBuilderFactory.newInstance();
            documentBuilderFactory.setFeature(XMLConstants.FEATURE_SECURE_PROCESSING, true);
            DocumentBuilder documentBuilder = documentBuilderFactory.newDocumentBuilder();

            Document svgDocument = documentBuilder.newDocument();
            Element svgElement = (Element) svgDocument.importNode(element, true);
            svgDocument.appendChild(svgElement);

            return Optional.of(new InlinedSvgElement(svgDocument, cssWidth, cssHeight));
        } catch (ParserConfigurationException exception) {
            Exceptions.handle(exception);
            return Optional.empty();
        }
    }

    private Optional<ReplacedElement> tryCreateReplacedImageElement(@Nonnull Element element,
                                                                    UserAgentCallback userAgentCallback,
                                                                    int cssWidth,
                                                                    int cssHeight) {
        String nodeName = element.getNodeName();
        if (!TAG_TYPE_IMG.equals(nodeName)) {
            return Optional.empty();
        }

        String source = rewriteLegacyUrl(element.getAttribute(ATTR_SRC));
        if (Strings.isEmpty(source)) {
            return Optional.empty();
        }

        try {
            String protocol = Strings.split(source, "://").getFirst();
            PdfReplaceHandler handler = findHandler(protocol);
            return Optional.of(new AsyncLoadedImageElement(handler, userAgentCallback, source, cssWidth, cssHeight));
        } catch (Exception exception) {
            Exceptions.handle(exception);
            return Optional.empty();
        }
    }

    private PdfReplaceHandler findHandler(String protocol) {
        return handlers.stream()
                       .filter(handler -> handler.accepts(protocol))
                       .findFirst()
                       .orElseThrow(() -> new UnsupportedOperationException(Strings.apply(
                               "No handler for protocol '%s' could be found",
                               protocol)));
    }

    private String rewriteLegacyUrl(String url) {
        for (PdfReplaceHandler handler : handlers) {
            Optional<String> rewrittenUrl = handler.tryRewritePlainUrl(url);
            if (rewrittenUrl.isPresent()) {
                return rewrittenUrl.get();
            }
        }

        return url;
    }
}
