/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.mails;

import sirius.web.templates.Resource;

import java.io.IOException;
import java.io.InputStream;

/**
 * Encapsulates all information required to add an attachment to a mail being sent.
 */
public class ResourceAttachment extends Attachment {

    private Resource resource;

    /**
     * Generates a new attachment with the given name, mime type and contents.
     *
     * @param name          the name of the attached file
     * @param mimeType      the mime type of the file. Use {@link sirius.web.http.MimeHelper} to determine it at
     *                      runtime if it is not know in advance
     * @param resource      the resource to use as contents of the template
     * @param asAlternative determines if this attachment is an alternative to the text content of the mail
     *                      (<tt>true</tt>) or a real attachment (<tt>false</tt>)
     */
    public ResourceAttachment(String name, String mimeType, Resource resource, boolean asAlternative) {
        super(name, mimeType, asAlternative);
        this.resource = resource;
    }

    @Override
    public InputStream getInputStream() throws IOException {
        return resource.openStream();
    }
}
