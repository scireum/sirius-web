/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates;

import sirius.kernel.di.std.Part;

import javax.annotation.Nonnull;
import java.util.Optional;

/**
 * Provides helper methods used in Velocity Macros.
 * <p>
 * The instance of this class is made available as <b>helper</b>
 * via {@link sirius.web.templates.DefaultContentContextExtender}.
 */
public class ContentHelper {

    /**
     * Contains the instance which is passed into the velocity context as "helper".
     */
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

    @Part
    private static Resources resources;

    /**
     * Returns the contents of the given template as a single line string which can be embedded into a string
     * enclosed by ' (e.g. a JavaScript string).
     *
     * @param resource the template to fetch
     * @return the contents of the template without line breaks and with escaped ticks (<tt>'</tt>). If the template
     * cannot be found an empty string is returned
     */
    @Nonnull
    public String getResourceAsInlineString(String resource) {
        Optional<Resource> res = resources.resolve(resource);
        return res.map(r -> r.getContentAsString().replaceAll("\\r?\\n", " ").replace("'", "\\'")).orElse("");
    }
}
