package sirius.web.templates;

import com.google.common.base.Charsets;
import org.apache.velocity.app.Velocity;
import org.xhtmlrenderer.pdf.ITextRenderer;
import sirius.kernel.commons.Strings;
import sirius.kernel.di.std.Register;

import java.io.OutputStream;
import java.io.StringWriter;

/**
 * Generates a PDF output by evaluating a given velocity template which must result in a valid XHTML dom.
 * <p>
 * This handler expects velocity as template language which must generate a valid XHTML output.
 * This is post processed by flying saucer to generate a PDF file. The name of this handler is <b>pdf-vm</b>
 * the expected file extension is <b>.pdf.vm</b>.
 * </p>
 *
 * @author Andreas Haufler (aha@scireum.de)
 * @since 2014/02
 */
@Register(name = VelocityPDFContentHandler.PDF_VM)
public class VelocityPDFContentHandler implements ContentHandler {

    /**
     * Contains the name (type) of this handler
     */
    public static final String PDF_VM = "pdf-vm";

    @Override
    public boolean generate(Content.Generator generator, OutputStream out) throws Exception {
        if (!PDF_VM.equals(generator.getHandlerType()) && !generator.isTemplateEndsWith(".pdf.vm")) {
            return false;
        }

        ScriptingContext ctx = new ScriptingContext();
        generator.getContext().applyTo(ctx);

        StringWriter writer = new StringWriter();
        if (Strings.isFilled(generator.getTemplateCode())) {
            Velocity.evaluate(ctx, writer, "velocity", generator.getTemplateCode());
        } else {
            Velocity.mergeTemplate(generator.getTemplateName(), Charsets.UTF_8.name(), ctx, writer);
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
