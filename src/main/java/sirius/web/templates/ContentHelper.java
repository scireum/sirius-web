/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates;

import sirius.kernel.commons.Strings;
import sirius.kernel.di.std.Part;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.text.CharacterIterator;
import java.text.StringCharacterIterator;
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

    /**
     * Removes all XML characters (&lt;, &gt;, &quot;, &#039;, &amp;) from the input and replaces it with the well known
     * replacement characters.
     *
     * @param aText the text to replace
     * @return a string which can safely output in XML or HTML. Returns an empty string if the input was <tt>null</tt>.
     */
    @Nonnull
    public static String escapeXML(@Nullable Object aText) {
        if (Strings.isEmpty(aText)) {
            return "";
        }

        final StringBuilder result = new StringBuilder();
        final StringCharacterIterator iterator = new StringCharacterIterator(aText.toString());
        char character = iterator.current();
        while (character != CharacterIterator.DONE) {
            if (character == '<') {
                result.append("&lt;");
            } else if (character == '>') {
                result.append("&gt;");
            } else if (character == '\"') {
                result.append("&quot;");
            } else if (character == '\'') {
                result.append("&#039;");
            } else if (character == '&') {
                result.append("&amp;");
            } else {
                // the char is not a special one
                // add it to the result as is
                result.append(character);
            }
            character = iterator.next();
        }

        return result.toString();
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
