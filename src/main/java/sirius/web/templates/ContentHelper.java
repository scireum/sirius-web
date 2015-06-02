/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates;

/**
 * Provides helper methods used in Velocity Macros.
 * <p>
 * The instance of this class is made available as <b>helper</b>
 * via {@link sirius.web.templates.DefaultContentContextExtender}.
 */
public class ContentHelper {

    public static final ContentHelper INSTANCE = new ContentHelper();

    private ContentHelper() {
    }

    /**
     * Replaces new line with &lt;br&gt; tags.
     *
     * @param content the content to parse
     * @return the value of <b>content</b> where all line breaks are replaced by &lt;br&gt; tags.
     */
    public String nl2br(String content) {
        if (content == null) {
            return null;
        }
        return content.replace("\n", " <br> ");
    }
}
