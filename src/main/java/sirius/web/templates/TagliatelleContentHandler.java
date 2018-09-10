/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates;

import sirius.kernel.commons.Explain;
import sirius.kernel.commons.Strings;
import sirius.kernel.di.std.Part;
import sirius.kernel.di.std.Register;
import sirius.tagliatelle.Tagliatelle;
import sirius.tagliatelle.Template;
import sirius.tagliatelle.compiler.CompileException;
import sirius.tagliatelle.compiler.Compiler;

import java.io.OutputStream;
import java.io.OutputStreamWriter;

/**
 * Generates text output by evaluating a given Tagliatelle code.
 * <p>
 * This handler expects Tagliatelle as template language. The name of this handler is <b>pasta</b> the expected file
 * extension is <b>.pasta</b>
 */
@Register(name = TagliatelleContentHandler.PASTA)
public class TagliatelleContentHandler implements ContentHandler {

    /**
     * Contains the name (type) of this handler
     */
    public static final String PASTA = "pasta";

    @Part
    private Tagliatelle tagliatelle;

    @SuppressWarnings("squid:S2440")
    @Explain("False positive")
    protected Template getTemplate(Generator generator) throws CompileException {
        if (Strings.isFilled(generator.getTemplateCode())) {
            Compiler compiler = new Compiler(tagliatelle.createCompilationContext("inline", null, null),
                                             generator.getTemplateCode());
            compiler.compile();
            return compiler.getContext().getTemplate();
        } else {
            return tagliatelle.resolve(generator.getTemplateName()).orElse(null);
        }
    }

    @Override
    public boolean generate(Generator generator, OutputStream out) throws Exception {
        if (!PASTA.equals(generator.getHandlerType())
            && !Strings.isFilled(generator.getTemplateCode())
            && !generator.isTemplateEndsWith("." + PASTA)) {
            return false;
        }

        Template template = getTemplate(generator);
        if (template == null) {
            return false;
        }

        String content = template.renderWithParams(generator.getContext());
        try (OutputStreamWriter writer = new OutputStreamWriter(out, generator.getEncoding())) {
            writer.write(content);
        }

        return true;
    }

    @Override
    public int getPriority() {
        return 999;
    }
}
