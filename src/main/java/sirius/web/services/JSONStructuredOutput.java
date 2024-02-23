/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.services;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.fasterxml.jackson.databind.node.POJONode;
import sirius.kernel.commons.Amount;
import sirius.kernel.commons.Json;
import sirius.kernel.commons.Strings;
import sirius.kernel.health.Exceptions;
import sirius.kernel.xml.AbstractStructuredOutput;
import sirius.kernel.xml.Attribute;
import sirius.kernel.xml.StructuredOutput;

import javax.annotation.Nullable;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.UnsupportedEncodingException;
import java.io.Writer;

/**
 * Encoder to generate JSON via the {@link sirius.kernel.xml.StructuredOutput} interface.
 */
public class JSONStructuredOutput extends AbstractStructuredOutput {

    private final Writer writer;
    private final String callback;

    /**
     * Generates a new output, writing to the given output stream.
     *
     * @param out      the destination for the generated output
     * @param callback name of the callback function for JSONP requests
     * @param encoding the character encoding to use
     */
    public JSONStructuredOutput(OutputStream out, @Nullable String callback, String encoding) {
        try {
            this.callback = callback;
            writer = new OutputStreamWriter(out, encoding);
        } catch (UnsupportedEncodingException exception) {
            throw Exceptions.handle(exception);
        }
    }

    /**
     * Generates a new output, writing to the given writer.
     *
     * @param destination the destination for the generated output
     * @param callback    name of the callback function for JSONP requests
     */
    public JSONStructuredOutput(Writer destination, @Nullable String callback) {
        this.callback = callback;
        this.writer = destination;
    }

    @Override
    protected void endArray(String name) {
        try {
            writer.write("]");
        } catch (IOException exception) {
            throw handleOutputException(exception);
        }
    }

    @Override
    protected void endObject(String name) {
        try {
            writer.write("}");
        } catch (IOException exception) {
            throw handleOutputException(exception);
        }
    }

    @Override
    protected void startArray(String name) {
        try {
            addRequiredComma();
            if (getCurrentType() == ElementType.OBJECT) {
                writeString(name);
                writer.write(":[");
            } else {
                writer.write("[");
            }
        } catch (IOException exception) {
            throw handleOutputException(exception);
        }
    }

    @Override
    protected void startObject(String name, Attribute... attributes) {
        try {
            addRequiredComma();
            if (getCurrentType() == ElementType.OBJECT) {
                writeString(name);
                writer.write(":{");
            } else {
                writer.write("{");
            }
            if (attributes != null) {
                for (Attribute attr : attributes) {
                    property(attr.getName(), attr.getValue());
                }
            }
        } catch (IOException exception) {
            throw handleOutputException(exception);
        }
    }

    private void writeString(String value) throws IOException {
        if (value == null || value.length() == 0) {
            writer.append("\"\"");
            return;
        }

        writeQuotedString(value);
    }

    private void writeQuotedString(String value) throws IOException {
        writer.append("\"");
        int len = value.length();
        char currentCharacter = '\0';
        char previousCharacter;

        for (int i = 0; i < len; i++) {
            previousCharacter = currentCharacter;
            currentCharacter = value.charAt(i);
            if (shouldEscapeCharacter(previousCharacter, currentCharacter)) {
                writer.append('\\').append(currentCharacter);
            } else if (currentCharacter == '\b') {
                writer.append("\\b");
            } else if (currentCharacter == '\t') {
                writer.append("\\t");
            } else if (currentCharacter == '\n') {
                writer.append("\\n");
            } else if (currentCharacter == '\f') {
                writer.append("\\f");
            } else if (currentCharacter == '\r') {
                writer.append("\\r");
            } else if (isSpecialCharacter(currentCharacter)) {
                String t = "000" + Integer.toHexString(currentCharacter);
                writer.append("\\u" + t.substring(t.length() - 4));
            } else {
                writer.append(currentCharacter);
            }
        }
        writer.append("\"");
    }

    private boolean shouldEscapeCharacter(char previousCharacter, char currentCharacter) {
        // \ and " are control characters and have to be escaped...
        if (currentCharacter == '\\' || currentCharacter == '"') {
            return true;
        }

        // A forward slash may be escaped in JSON but doesn't have to. We only escape, if we find a </ as this will
        // confuse a browser even within a JSON string...
        return currentCharacter == '/' && previousCharacter == '<';
    }

    private boolean isSpecialCharacter(char currentCharacter) {
        if (isControlChar(currentCharacter)) {
            return true;
        }

        if (currentCharacter >= '\u0080' && currentCharacter < '\u00a0') {
            return true;
        }

        return currentCharacter >= '\u2000' && currentCharacter < '\u2100';
    }

    private boolean isControlChar(char currentCharacter) {
        return currentCharacter < ' ';
    }

    @Override
    public StructuredOutput beginResult() {
        try {
            if (Strings.isFilled(callback)) {
                writer.write(callback);
                writer.write("(");
            }
            beginObject("result");
        } catch (IOException exception) {
            throw handleOutputException(exception);
        }

        return this;
    }

    @Override
    public StructuredOutput beginResult(String name) {
        return beginResult();
    }

    @Override
    public void writeProperty(String name, Object data) {
        if (data instanceof JsonNode jsonNode) {
            writePreformattedProperty(name, Json.write(jsonNode));
        } else {
            writePlainProperty(name, data);
        }
    }

    private void writePreformattedProperty(String name, String value) {
        try {
            addRequiredComma();
            addObjectName(name);
            if (value == null) {
                writer.write("null");
                return;
            }
            writer.write(value);
        } catch (IOException exception) {
            throw handleOutputException(exception);
        }
    }

    private void writePlainProperty(String name, Object data) {
        try {
            addRequiredComma();
            addObjectName(name);
            if (data == null) {
                writer.write("null");
            } else if (data instanceof Amount amount) {
                if (amount.isFilled()) {
                    writer.write(amount.toMachineString());
                } else {
                    writer.write("null");
                }
            } else if (data instanceof Boolean || data instanceof Number) {
                writer.write(data.toString());
            } else {
                writeString(transformToStringRepresentation(data));
            }
        } catch (IOException exception) {
            throw handleOutputException(exception);
        }
    }

    @Override
    protected void writeAmountProperty(String name, String formattedAmount) {
        try {
            addRequiredComma();
            addObjectName(name);
            writer.write(Strings.isFilled(formattedAmount) ? formattedAmount : "null");
        } catch (IOException exception) {
            throw handleOutputException(exception);
        }
    }

    private void addObjectName(String name) throws IOException {
        if (getCurrentType() == ElementType.OBJECT) {
            writeString(name);
            writer.write(":");
        }
    }

    private void addRequiredComma() {
        if (!isCurrentObjectEmpty()) {
            try {
                writer.write(",");
            } catch (IOException exception) {
                throw handleOutputException(exception);
            }
        }
    }

    @Override
    public void endResult() {
        endObject();
        finalizeOutput();
    }

    /**
     * Finalizes the output and closes the stream.
     * <p>
     * In contrast to {@link #endResult()} this does not require an open result object.
     */
    public void finalizeOutput() {
        try {
            super.endResult();
            if (Strings.isFilled(callback)) {
                writer.write(")");
            }
            writer.close();
        } catch (IOException exception) {
            throw handleOutputException(exception);
        }
    }
}
