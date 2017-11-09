/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.services;

import com.alibaba.fastjson.serializer.SerializeWriter;
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

    private Writer writer;
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
        } catch (IOException e) {
            throw Exceptions.handle(e);
        }
    }

    @Override
    protected void endObject(String name) {
        try {
            writer.write("}");
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

    private void writeString(String value) {
        try (SerializeWriter jsonWriter = new SerializeWriter(writer)) {
            jsonWriter.writeStringWithDoubleQuote(value, (char) 0);
        }
    }


    @Override
    public StructuredOutput beginResult() {
        try {
            if (Strings.isFilled(callback)) {
                writer.write(callback);
                writer.write("(");
            }
            beginObject("result");
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
            } else {
                writeString(data.toString());
            }
        } catch (IOException e) {
            throw Exceptions.handle(e);
        }
    }

    private void addRequiredComma() {
        if (!isCurrentObjectEmpty()) {
            try {
                writer.write(",");
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
        } catch (IOException e) {
            throw Exceptions.handle(e);
        }
    }
}
