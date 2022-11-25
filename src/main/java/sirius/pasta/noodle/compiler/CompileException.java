/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.compiler;

import sirius.kernel.commons.Files;
import sirius.pasta.tagliatelle.compiler.TemplateCompiler;
import sirius.web.services.JSONStructuredOutput;

import javax.annotation.Nullable;
import java.io.Serial;
import java.util.Collections;
import java.util.List;

/**
 * Thrown to indicate one or more {@link CompileError compilation errors}.
 * <p>
 * As the compiler is quite optimistic, it keeps compiling as long as possible instead of aborting for the first error.
 * Therefore, a list of errors is given here which is also appropriately formatted in the exception message.
 */
public class CompileException extends Exception {

    @Serial
    private static final long serialVersionUID = -8697032594602395681L;

    private final transient List<CompileError> errors;

    private CompileException(String message, List<CompileError> errors) {
        super(message);
        this.errors = errors;
    }

    /**
     * Creates a new exception based on the list of errors.
     *
     * @param sourceName     the name of the source file being processed
     * @param sourceLocation the location (URL / path) of the compiled sources
     * @param errors         the errors which occurred while processing the user input
     * @return a new CompileException which can be thrown
     */
    public static CompileException create(String sourceName,
                                          @Nullable String sourceLocation,
                                          List<CompileError> errors) {
        StringBuilder message = new StringBuilder();
        message.append("Cannot compile ").append(Files.getFilenameAndExtension(sourceName)).append(":\n");
        errors.forEach(message::append);
        message.append("Source: ");
        message.append(sourceName);
        message.append("\n");

        if (sourceLocation != null) {
            message.append("Location: ");
            message.append(sourceLocation);
            message.append("\n");
        }

        return new CompileException(message.toString(), errors);
    }

    /**
     * Provides a list of all errors and warnings which occurred
     *
     * @return all errors and warnings which occurred while processing the user input
     */
    public List<CompileError> getErrors() {
        return Collections.unmodifiableList(errors);
    }

    /**
     * Reports all collected errors as JSON, which can be processed by the ACE editor.
     *
     * @param out the JSON output to write to
     */
    public void reportAsJSON(JSONStructuredOutput out) {
        TemplateCompiler.reportAsJson(getErrors().stream().map(CompileError::getError).toList(), out);
    }
}
