/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle;

import parsii.tokenizer.ParseError;
import sirius.kernel.Sirius;
import sirius.kernel.TestLifecycleParticipant;
import sirius.kernel.commons.Strings;
import sirius.kernel.di.std.Part;
import sirius.kernel.di.std.Priorized;
import sirius.kernel.di.std.Register;
import sirius.kernel.health.Exceptions;
import sirius.tagliatelle.compiler.CompilationContext;
import sirius.tagliatelle.compiler.CompileException;
import sirius.tagliatelle.compiler.Compiler;
import sirius.web.resources.Resource;
import sirius.web.resources.Resources;

import java.util.IntSummaryStatistics;
import java.util.Objects;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

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
        StringBuilder output = new StringBuilder();
        IntSummaryStatistics statistics = Sirius.getClasspath()
                                                .find(Pattern.compile(".*.pasta"))
                                                .map(this::resolveAsResource)
                                                .filter(Objects::nonNull)
                                                .map(this::compile)
                                                .filter(this::isExpectedToCompile)
                                                .filter(this::hasErrorsOrWarnings)
                                                .peek(compilationContext -> reportErrors(compilationContext, output))
                                                .mapToInt(compilationContext -> compilationContext.getErrors().size())
                                                .summaryStatistics();

        if (statistics.getCount() > 0) {
            throw Exceptions.handle()
                            .withSystemErrorMessage(
                                    "ReportBrokenTemplates found %s Tagliatelle template(s) with %s error(s)/warning(s):\n\n%s",
                                    statistics.getCount(),
                                    statistics.getSum(),
                                    output.toString())
                            .handle();
        }
    }

    private Resource resolveAsResource(Matcher matchedResource) {
        String templateName = "/" + matchedResource.group(0);
        if (templateName.startsWith("/default/")) {
            templateName = templateName.substring(8);
        }

        return resources.resolve(templateName).orElse(null);
    }

    private CompilationContext compile(Resource resource) {
        CompilationContext compilationContext = engine.createCompilationContext(resource.getPath(), resource, null);

        try {
            Compiler compiler = new Compiler(compilationContext, resource.getContentAsString());
            compiler.compile();
        } catch (CompileException e) {
            // We will log all errors later anyway...
            Exceptions.ignore(e);
        }

        return compilationContext;
    }

    private boolean isExpectedToCompile(CompilationContext compilationContext) {
        return !compilationContext.getTemplate().getPragma(PRAGMA_UNCHECKED).asBoolean();
    }

    private boolean hasErrorsOrWarnings(CompilationContext compilationContext) {
        return !compilationContext.getErrors().isEmpty();
    }

    private void reportErrors(CompilationContext compilationContext, StringBuilder output) {
        output.append(Strings.apply("%s:\n%s\n\n",
                                    compilationContext.getTemplate().getName(),
                                    compilationContext.getErrors()
                                                      .stream()
                                                      .map(ParseError::toString)
                                                      .collect(Collectors.joining("\n"))));
    }
}
