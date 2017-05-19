/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle;

import java.util.List;

/**
 * Created by aha on 12.05.17.
 */
public class CompileException extends Exception {

    private static final long serialVersionUID = -8697032594602395681L;

    private final Template template;
    private final transient List<CompileError> errors;

    private CompileException(String message, Template template, List<CompileError> errors) {
        super(message);
        this.template = template;
        this.errors = errors;
    }

    /**
     * Creates a new exception based on the list of errors.
     *
     * @param errors the errors which occurred while processing the user input
     * @return a new ParseException which can be thrown
     */
    public static CompileException create(Template template, List<CompileError> errors) {
        StringBuilder message = new StringBuilder();
        message.append("Cannot compile: ")
               .append(template.getName())
               .append(" (")
               .append(template.getResource().getUrl())
               .append("):\n");
        errors.forEach(message::append);

        return new CompileException(message.toString(), template, errors);
    }

    /**
     * Provides a list of all errors and warnings which occurred
     *
     * @return all errors and warnings which occurred while processing the user input
     */
    public List<CompileError> getErrors() {
        return errors;
    }

    /**
     * Returns the template for which the compilation failed.
     *
     * @return the template which was compiled
     */
    public Template getTemplate() {
        return template;
    }
}

