/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates.pdf;

import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Entities;
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
        String cleanedContent = cleanHtml(content);

        ITextRenderer renderer = new ITextRenderer();
        renderer.getSharedContext()
                .setReplacedElementFactory(new ImageReplacedElementFactory(renderer.getOutputDevice()));
        renderer.setDocumentFromString(cleanedContent);
        renderer.layout();
        renderer.createPDF(out);
        out.flush();

        return true;
    }

    /**
     * Cleans the given HTML content for use as input to the PDF generator.
     * <p>
     * This is done by first generally removing all {@code <script>} elements from the entire document. Then, all
     * {@code <style>} elements are deleted that are outside the {@code <header>} element. Finally, the DOM tree
     * is encoded as XHTML fit for the strict SAX parser employed by {@link ITextRenderer}.
     *
     * @param html the HTML content to clean
     * @return the given content with problematic elements removed and encoded as valid XHTML
     */
    private String cleanHtml(String html) {
        Document document = Jsoup.parse(html);

        // the parser is very strict in terms of what elements are accepted within the <head> element, and it does not
        // know about additional valid elements like <bookmarks> that are valid for flying saucer; we need to move them
        // back from the <body> to the <head> element
        document.select("body > bookmarks").forEach(bookmarks -> {
            document.head().appendChild(bookmarks);
        });

        // in theory, the following two lines should be possible with a single CSS selector; however, in practice, Jsoup
        // does not select the style elements correctly when attempting that
        document.select("script").remove();
        document.select("body style").remove();

        document.outputSettings().syntax(Document.OutputSettings.Syntax.xml);
        document.outputSettings().escapeMode(Entities.EscapeMode.xhtml);
        document.outputSettings().charset("UTF-8");
        return document.html();
    }

    @Override
    public int getPriority() {
        return DEFAULT_PRIORITY;
    }
}
