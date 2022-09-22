/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.sass;

import java.io.IOException;
import java.io.Writer;

/**
 * Output layer used to generate more or less formatted output.
 * <p>
 * This is used by the {@link Generator} to create the final output. Depending on the settings, the output will
 * be nicely formatted or "minified".
 */
public class Output {
    protected int indentLevel = 0;
    protected String indentDepth = "    ";
    protected Writer writer;
    protected boolean skipOptionalOutput;

    /**
     * Creates a new output for the given writer and output settings.
     *
     * @param writer             the target of the generated output
     * @param skipOptionalOutput determines if the output should be properly indented or "minified".
     */
    public Output(Writer writer, boolean skipOptionalOutput) {
        this.writer = writer;
        this.skipOptionalOutput = skipOptionalOutput;
    }

    /**
     * Outputs the given value as string.
     * <p>
     * This will be added to the output in any case.
     *
     * @param value the data out add to the output
     * @return the instance itself for fluent method calls
     * @throws IOException in case of an io error in the underlying writer
     */
    public Output output(Object value) throws IOException {
        if (value != null) {
            writer.write(value.toString());
        }
        return this;
    }

    /**
     * Outputs an indented line break or a " " in case of minified output.
     *
     * @return the instance itself for fluent method calls
     * @throws IOException in case of an io error in the underlying writer
     */
    public Output optionalLineBreak() throws IOException {
        if (skipOptionalOutput) {
            writer.write(" ");
        } else {
            writer.write("\n");
            for (int i = 0; i < indentLevel; i++) {
                writer.write(indentDepth);
            }
        }

        return this;
    }

    /**
     * Outputs an indented line break in any case.
     *
     * @return the instance itself for fluent method calls
     * @throws IOException in case of an io error in the underlying writer
     */
    public Output lineBreak() throws IOException {
        writer.write("\n");
        if (!skipOptionalOutput) {
            for (int i = 0; i < indentLevel; i++) {
                writer.write(indentDepth);
            }
        }
        return this;
    }

    /**
     * Increments the indentation.
     *
     * @return the instance itself for fluent method calls
     */
    public Output incIndent() {
        indentLevel++;

        return this;
    }

    /**
     * Decrements the indentation.
     *
     * @return the instance itself for fluent method calls
     */
    public Output decIndent() {
        indentLevel--;

        return this;
    }

    @Override
    public String toString() {
        return writer.toString();
    }
}
