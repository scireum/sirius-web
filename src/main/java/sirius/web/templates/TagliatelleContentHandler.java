/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates;

import com.google.common.base.Charsets;
import sirius.kernel.commons.Strings;
import sirius.kernel.di.std.Register;
import sirius.web.templates.velocity.VelocityHelper;

import java.io.OutputStream;
import java.io.OutputStreamWriter;


@Register(name = TagliatelleContentHandler.VM)
public class TagliatelleContentHandler implements ContentHandler {

    /**
     * Contains the name (type) of this handler
     */
    public static final String VM = "vm";

    @Override
    public boolean generate(Generator generator, OutputStream out) throws Exception {
        if (!VM.equals(generator.getHandlerType())
            && !Strings.isFilled(generator.getTemplateCode())
            && !generator.isTemplateEndsWith("." + VM)) {
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
