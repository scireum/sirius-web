/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates.pdf;

import org.xhtmlrenderer.pdf.ITextRenderer;
import sirius.kernel.di.std.Part;
import sirius.kernel.di.std.Register;
import sirius.pasta.tagliatelle.Tagliatelle;
import sirius.pasta.tagliatelle.Template;
import sirius.web.templates.ContentHandler;
import sirius.web.templates.Generator;
import sirius.web.templates.TagliatelleContentHandler;

import java.io.OutputStream;

/**
 * Generates a PDF output by evaluating a given tagliatelle template which must result in a valid XHTML dom.
 * <p>
 * This handler expects tagliatelle as template language which must generate a valid XHTML output.
 * This is post processed by flying saucer to generate a PDF file. The name of this handler is <b>pdf-pasta</b>
 * the expected file extension is <b>.pdf.pasta</b>.
 */
@Register(name = TagliatellePDFContentHandler.PDF_PASTA, classes = ContentHandler.class)
public class TagliatellePDFContentHandler extends TagliatelleContentHandler {

    /**
     * Contains the name (type) of this handler
     */
    public static final String PDF_PASTA = "pdf-pasta";

    @Part
    private Tagliatelle tagliatelle;

    @Override
    public boolean generate(Generator generator, OutputStream out) throws Exception {
        if (!PDF_PASTA.equals(generator.getHandlerType()) && !generator.isTemplateFileExtension("pdf.pasta")) {
            return false;
        }

        Template template = getTemplate(generator);
        if (template == null) {
            return false;
        }

        String content = template.renderWithParams(generator.getContext());

        ITextRenderer renderer = new ITextRenderer();
        renderer.getSharedContext()
                .setReplacedElementFactory(new ImageReplacedElementFactory(renderer.getOutputDevice()));
        renderer.setDocumentFromString(content);
        renderer.layout();
        renderer.createPDF(out);
        out.flush();

        return true;
    }

    @Override
    public int getPriority() {
        return DEFAULT_PRIORITY;
    }
}
