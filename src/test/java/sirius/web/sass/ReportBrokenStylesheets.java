/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.sass;

import sirius.kernel.Sirius;
import sirius.kernel.TestLifecycleParticipant;
import sirius.kernel.commons.Strings;
import sirius.kernel.di.GlobalContext;
import sirius.kernel.di.std.Part;
import sirius.kernel.di.std.Priorized;
import sirius.kernel.di.std.Register;
import sirius.kernel.health.Exceptions;
import sirius.web.dispatch.SassFunction;
import sirius.web.resources.Resource;
import sirius.web.resources.Resources;
import sirius.web.sass.ast.Expression;
import sirius.web.sass.ast.FunctionCall;
import sirius.web.sass.ast.Value;

import java.io.IOException;
import java.io.InputStream;
import java.util.IntSummaryStatistics;
import java.util.LinkedList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Tries to compile all visible SASS stylesheets and breaks the test run if compilation fails.
 */
@Register
public class ReportBrokenStylesheets implements TestLifecycleParticipant {

    private record CompilationResult(String path, boolean parsingError, boolean functionEvaluationError,
                                     List<String> warnings) {
    }

    @Part
    private Resources resources;

    @Part
    private GlobalContext globalContext;

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
                                                .find(Pattern.compile(".*.scss"))
                                                .map(this::resolveAsResource)
                                                .filter(Objects::nonNull)
                                                .filter(this::isLocalStylesheet)
                                                .map(this::compile)
                                                .filter(this::hasErrorsOrWarnings)
                                                .peek(compilationResult -> reportErrors(compilationResult, output))
                                                .mapToInt(compilationResult -> compilationResult.warnings().size())
                                                .summaryStatistics();

        if (statistics.getCount() > 0) {
            throw Exceptions.handle()
                            .withSystemErrorMessage(
                                    "ReportBrokenStylesheets found %s SASS stylesheet(s) with %s error(s)/warning(s):\n\n%s",
                                    statistics.getCount(),
                                    statistics.getSum(),
                                    output.toString())
                            .handle();
        }
    }

    private boolean isLocalStylesheet(Resource resource) {
        return !resource.getUrl().toString().startsWith("jar:file:") && !resource.getPath().contains("/libs/");
    }

    private Resource resolveAsResource(Matcher matchedResource) {
        String templateName = "/" + matchedResource.group(0);
        if (templateName.startsWith("/default/")) {
            templateName = templateName.substring(8);
        }

        return resources.resolve(templateName).orElse(null);
    }

    private CompilationResult compile(Resource resource) {
        List<String> warnings = new LinkedList<>();
        AtomicBoolean functionEvaluationError = new AtomicBoolean(false);

        Generator generator = new Generator() {

            @Override
            public void warn(String message) {
                warnings.add(message);
            }

            @Override
            protected InputStream resolveIntoStream(String sheet) throws IOException {
                Optional<Resource> res = resources.resolve(sheet);
                if (res.isPresent()) {
                    return res.get().getUrl().openStream();
                }
                return null;
            }

            @Override
            public Expression evaluateFunction(FunctionCall call) {
                SassFunction sassFunction = globalContext.getPart(call.getName(), SassFunction.class);
                if (sassFunction != null) {
                    try {
                        return new Value(sassFunction.eval(call));
                    } catch (Exception e) {
                        functionEvaluationError.set(true);
                        warn("Cannot execute function: " + call + " - " + e.getMessage());
                        return super.evaluateFunction(call);
                    }
                }

                return super.evaluateFunction(call);
            }
        };

        // a warning at this point is a parsing error, or a missing imported file
        generator.importStylesheet(resource.getPath());
        boolean parsingError = !warnings.isEmpty();

        // compile in order to check function evaluations; note, however, that missing variables may pose problems
        generator.compile();

        return new CompilationResult(resource.getPath(), parsingError, functionEvaluationError.get(), warnings);
    }

    private boolean hasErrorsOrWarnings(CompilationResult result) {
        return result.parsingError || result.functionEvaluationError;
    }

    private void reportErrors(CompilationResult result, StringBuilder output) {
        output.append(Strings.apply("%s:\n%s\n\n", result.path, String.join("\n", result.warnings)));
    }
}
