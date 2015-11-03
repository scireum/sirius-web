/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates.velocity;

import com.google.common.base.Charsets;
import sirius.kernel.commons.Strings;
import sirius.kernel.di.std.Register;
import sirius.web.templates.ContentHandler;
import sirius.web.templates.ScriptingContext;
import sirius.web.templates.Templates;

import java.io.OutputStream;
import java.io.OutputStreamWriter;

/**
 * Generates text output by evaluating a given velocity template.
 * <p>
 * This handler expects velocity as template language. The name of this handler is <b>vm</b> the expected file
 * extension
 * is
 * <b>.vm</b>. It has a priority of 999 so that it does not collide with other handlers like
 * {@link VelocityPDFContentHandler} which expects <b>.pdf.vm</b>
 */
@Register(name = VelocityContentHandler.VM)
public class VelocityContentHandler implements ContentHandler {

    /**
     * Contains the name (type) of this handler
     */
    public static final String VM = "vm";

    @Override
    public boolean generate(Templates.Generator generator, OutputStream out) throws Exception {
        if (!VM.equals(generator.getHandlerType())
            && !Strings.isFilled(generator.getTemplateCode())
            && !generator.isTemplateFileExtension("vm")) {
            return false;
        }

        ScriptingContext ctx = new ScriptingContext();
        generator.getContext().applyTo(ctx);

        OutputStreamWriter writer = new OutputStreamWriter(out, generator.getEncoding());
        if (Strings.isFilled(generator.getTemplateCode())) {
            VelocityHelper.getEngine().evaluate(ctx, writer, "velocity", generator.getTemplateCode());
        } else {
            VelocityHelper.getEngine().mergeTemplate(generator.getTemplateName(), Charsets.UTF_8.name(), ctx, writer);
        }
        writer.close();

        return true;
    }

    @Override
    public int getPriority() {
        return 999;
    }
}
