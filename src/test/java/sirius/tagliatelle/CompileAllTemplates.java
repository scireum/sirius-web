/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle;

import sirius.kernel.Lifecycle;
import sirius.kernel.Sirius;
import sirius.kernel.di.std.Part;
import sirius.kernel.di.std.Register;
import sirius.kernel.health.Exceptions;
import sirius.tagliatelle.compiler.CompileException;

import java.util.regex.Pattern;

/**
 * Created by aha on 14.06.17.
 */
@Register
public class CompileAllTemplates implements Lifecycle {

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
                Exceptions.ignore(e);
                Tagliatelle.LOG.INFO("Failed to compile %s: %s", templateName, e.getMessage());
            }
        });
    }

    @Override
    public void stopped() {

    }

    @Override
    public void awaitTermination() {

    }

    @Override
    public String getName() {
        return "CompileAllTemplates";
    }
}
