/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.mails;

import javax.activation.DataSource;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

/**
 * Encapsulates all information required to add an attachment to a mail being sent.
 *
 * @author Andreas Haufler (aha@scireum.de)
 * @since 2014/02
 */
class Attachment implements DataSource {

    private String contentType;
    private byte[] buffer;
    private String name;

    /**
     * Generates a new attachment with the given name, mime type and contents.
     *
     * @param name      the name of the attached file
     * @param mimeType  the mime type of the file. Use {@link sirius.web.http.MimeHelper} to determine it at runtime if
     *                  it is not know in advance
     * @param byteArray the contents of the template
     */
    public Attachment(String name, String mimeType, byte[] byteArray) {
        this.name = name;
        contentType = mimeType;
        buffer = byteArray;
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
}
