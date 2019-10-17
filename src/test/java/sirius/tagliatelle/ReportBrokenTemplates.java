/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle;

import sirius.kernel.Sirius;
import sirius.kernel.TestLifecycleParticipant;
import sirius.kernel.di.std.Part;
import sirius.kernel.di.std.Priorized;
import sirius.kernel.di.std.Register;
import sirius.kernel.health.Exceptions;
import sirius.tagliatelle.compiler.CompilationContext;
import sirius.tagliatelle.compiler.CompileException;
import sirius.tagliatelle.compiler.Compiler;
import sirius.web.resources.Resource;
import sirius.web.resources.Resources;

import java.util.regex.Pattern;

/**
 * Tries to compile all visible tagliatelle templates and breaks the test run if compilation fails.
 */
@Register
public class ReportBrokenTemplates implements TestLifecycleParticipant {

    /**
     * This pragma can be used to mark a template as unchecked.
     * <p>
     * Such templates may contain or emit warnings while being compiled without breaking the tests.
     */
    public static final String PRAGMA_UNCHECKED = "unchecked";

    @Part
    private Tagliatelle engine;

    @Part
    private Resources resources;

    @Override
    public int getPriority() {
        return Priorized.DEFAULT_PRIORITY;
    }

    @Override
    public void beforeTests() {
        // No setup required
    }

    @Override
    public void afterTests() {
        Sirius.getClasspath().find(Pattern.compile(".*.pasta")).forEach(m -> {
            String templateName = "/" + m.group(0);
            if (templateName.startsWith("/default/")) {
                templateName = templateName.substring(8);
            }
            Resource resource = resources.resolve(templateName).orElse(null);
            CompilationContext compilationContext = engine.createCompilationContext(templateName, resource, null);
            try {
                Compiler compiler = new Compiler(compilationContext, resource.getContentAsString());
                compiler.compile();
                // If no exception was thrown but still some warnings are present, we
                // will still break the test unless the template is marked as "unchecked".
                if (!compiler.getContext().getErrors().isEmpty() && !isUnchecked(compilationContext.getTemplate())) {
                    throw Exceptions.handle()
                                    .withSystemErrorMessage("A warning ocurred while trying to compile: %s - %s."
                                                            + " Please fix the reported issue or mark the template as unchecked"
                                                            + " via a pragma.",
                                                            templateName,
                                                            compiler.getContext().getErrors().get(0).toString())
                                    .handle();
                }
            } catch (CompileException e) {
                if (!isUnchecked(compilationContext.getTemplate())) {
                    throw Exceptions.handle()
                                    .withSystemErrorMessage("Failed to compile: %s - %s",
                                                            templateName,
                                                            e.getErrors().get(0).toString())
                                    .handle();
                }
            }
        });
    }

    private boolean isUnchecked(Template template) {
        return template.getPragma(PRAGMA_UNCHECKED).asBoolean();
    }
}
