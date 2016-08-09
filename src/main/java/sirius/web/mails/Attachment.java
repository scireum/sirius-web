/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.mails;

import com.google.common.collect.Maps;

import javax.activation.DataSource;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Map;
import java.util.Set;

/**
 * Encapsulates all information required to add an attachment to a mail being sent.
 */
public class Attachment implements DataSource {

    private String contentType;
    private byte[] buffer;
    private boolean asAlternative;
    private String name;
    private Map<String, String> headers = Maps.newTreeMap();

    /**
     * Generates a new attachment with the given name, mime type and contents.
     *
     * @param name          the name of the attached file
     * @param mimeType      the mime type of the file. Use {@link sirius.web.http.MimeHelper} to determine it at
     *                      runtime
     *                      if
     *                      it is not know in advance
     * @param byteArray     the contents of the template
     * @param asAlternative determines if this attachment is an alternative to the text content of the mail
     *                      (<tt>true</tt>) or a real attachment (<tt>false</tt>)
     */
    public Attachment(String name, String mimeType, byte[] byteArray, boolean asAlternative) {
        this.name = name;
        contentType = mimeType;
        buffer = byteArray;
        this.asAlternative = asAlternative;
    }

    /**
     * Adds a header to the attachment.
     *
     * @param name  the name of the header
     * @param value the value of the header
     * @return the attachment itself to support fluent method calls
     */
    public Attachment addHeader(String name, String value) {
        headers.put(name, value);
        return this;
    }

    @Override
    public String getContentType() {
        return contentType;
    }

    @Override
    public InputStream getInputStream() throws IOException {
        return new ByteArrayInputStream(buffer);
    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    public OutputStream getOutputStream() throws IOException {
        return null;
    }

    /**
     * Returns all headers declared for this attachment.
     *
     * @return all headers for this attachment
     */
    public Set<Map.Entry<String, String>> getHeaders() {
        return headers.entrySet();
    }

    /**
     * Determines if this is an "alternative" to the text content of the mail or a "real" attachment
     *
     * @return <tt>true</tt> if this is an alternative, <tt>false</tt> if this is a real attachment
     */
    public boolean isAlternative() {
        return asAlternative;
    }
}
