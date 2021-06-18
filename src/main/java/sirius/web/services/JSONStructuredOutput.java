/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.services;

import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import sirius.kernel.commons.Amount;
import sirius.kernel.commons.Strings;
import sirius.kernel.health.Exceptions;
import sirius.kernel.health.HandledException;
import sirius.kernel.xml.AbstractStructuredOutput;
import sirius.kernel.xml.Attribute;
import sirius.kernel.xml.StructuredOutput;

import javax.annotation.Nullable;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.UnsupportedEncodingException;
import java.io.Writer;
import java.nio.channels.ClosedChannelException;

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
        } catch (UnsupportedEncodingException e) {
            throw Exceptions.handle(e);
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
        } catch (ClosedChannelException e) {
            throw handleClosedChannel(e);
        } catch (IOException e) {
            throw Exceptions.handle(e);
        }
    }

    private HandledException handleClosedChannel(ClosedChannelException e) {
        return Exceptions.createHandled()
                         .error(e)
                         .withSystemErrorMessage("An IO exception occurred (closed channel): %s")
                         .handle();
    }

    @Override
    protected void endObject(String name) {
        try {
            writer.write("}");
        } catch (ClosedChannelException e) {
            throw handleClosedChannel(e);
        } catch (IOException e) {
            throw Exceptions.handle(e);
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
        } catch (ClosedChannelException e) {
            throw handleClosedChannel(e);
        } catch (IOException e) {
            throw Exceptions.handle(e);
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
        } catch (IOException e) {
            throw Exceptions.handle(e);
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
        } catch (ClosedChannelException e) {
            throw handleClosedChannel(e);
        } catch (IOException e) {
            throw Exceptions.handle(e);
        }

        return this;
    }

    @Override
    public StructuredOutput beginResult(String name) {
        return beginResult();
    }

    @Override
    public void writeProperty(String name, Object data) {
        if (data instanceof JSONObject) {
            beginObject(name);
            ((JSONObject) data).forEach(this::property);
            endObject();
        } else if (data instanceof JSONArray) {
            beginArray(name);
            ((JSONArray) data).forEach(element -> property("", element));
            endArray();
        } else {
            writePlainProperty(name, data);
        }
    }

    private void writePlainProperty(String name, Object data) {
        try {
            addRequiredComma();
            if (getCurrentType() == ElementType.OBJECT) {
                writeString(name);
                writer.write(":");
            }
            if (data == null) {
                writer.write("null");
            } else if (data instanceof Boolean || data instanceof Number) {
                writer.write(data.toString());
            } else if (data instanceof Amount) {
                writer.write(((Amount) data).toMachineString());
            } else {
                writeString(transformToStringRepresentation(data));
            }
        } catch (ClosedChannelException e) {
            throw handleClosedChannel(e);
        } catch (IOException e) {
            throw Exceptions.handle(e);
        }
    }

    private void addRequiredComma() {
        if (!isCurrentObjectEmpty()) {
            try {
                writer.write(",");
            } catch (ClosedChannelException e) {
                throw handleClosedChannel(e);
            } catch (IOException e) {
                throw Exceptions.handle(e);
            }
        }
    }

    @Override
    public void endResult() {
        try {
            endObject();
            super.endResult();
            if (Strings.isFilled(callback)) {
                writer.write(")");
            }
            writer.close();
        } catch (ClosedChannelException e) {
            throw handleClosedChannel(e);
        } catch (IOException e) {
            throw Exceptions.handle(e);
        }
    }
}
