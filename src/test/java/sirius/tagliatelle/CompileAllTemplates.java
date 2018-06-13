/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle;

import sirius.kernel.Sirius;
import sirius.kernel.Startable;
import sirius.kernel.di.std.Part;
import sirius.kernel.di.std.Register;
import sirius.tagliatelle.compiler.CompileException;

import java.util.regex.Pattern;

/**
 * Compiles all templates during a test cycle to detect errors as early as possible.
 */
@Register
public class CompileAllTemplates implements Startable {

    @Part
    private Tagliatelle engine;

    @Override
    public void started() {
        Sirius.getClasspath().find(Pattern.compile(".*.pasta")).forEach(m -> {
            String templateName = "/" + m.group(0);
            try {
                if (templateName.startsWith("/default/")) {
                    templateName = templateName.substring(8);
                }
                Tagliatelle.LOG.INFO("Checking: %s", templateName);
                engine.resolve(templateName);
            } catch (CompileException e) {
                Tagliatelle.LOG.INFO(e.getMessage());
            }
        });
    }
}
