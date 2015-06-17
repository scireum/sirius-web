/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates.velocity;

import com.google.common.base.Charsets;
import org.xhtmlrenderer.pdf.ITextRenderer;
import sirius.kernel.commons.Strings;
import sirius.kernel.di.std.Register;
import sirius.web.templates.ContentHandler;
import sirius.web.templates.ScriptingContext;
import sirius.web.templates.Templates;

import java.io.OutputStream;
import java.io.StringWriter;

/**
 * Generates a PDF output by evaluating a given velocity template which must result in a valid XHTML dom.
 * <p>
 * This handler expects velocity as template language which must generate a valid XHTML output.
 * This is post processed by flying saucer to generate a PDF file. The name of this handler is <b>pdf-vm</b>
 * the expected file extension is <b>.pdf.vm</b>.
 */
@Register(name = VelocityPDFContentHandler.PDF_VM)
public class VelocityPDFContentHandler implements ContentHandler {

    /**
     * Contains the name (type) of this handler
     */
    public static final String PDF_VM = "pdf-vm";

    @Override
    public boolean generate(Templates.Generator generator, OutputStream out) throws Exception {
        if (!PDF_VM.equals(generator.getHandlerType()) && !generator.isTemplateEndsWith(".pdf.vm")) {
            return false;
        }

        ScriptingContext ctx = new ScriptingContext();
        generator.getContext().applyTo(ctx);

        StringWriter writer = new StringWriter();
        if (Strings.isFilled(generator.getTemplateCode())) {
            VelocityHelper.getEngine().evaluate(ctx, writer, "velocity", generator.getTemplateCode());
        } else {
            VelocityHelper.getEngine().mergeTemplate(generator.getTemplateName(), Charsets.UTF_8.name(), ctx, writer);
        }

        ITextRenderer renderer = new ITextRenderer();
        renderer.getSharedContext()
                .setReplacedElementFactory(new BarcodeReplacedElementFactory(renderer.getOutputDevice()));
        renderer.setDocumentFromString(writer.toString());
        renderer.layout();
        renderer.createPDF(out);
        out.flush();
        writer.close();

        return true;
    }

    @Override
    public int getPriority() {
        return DEFAULT_PRIORITY;
    }
}
