/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates;

import sirius.kernel.commons.StringCleanup;
import sirius.kernel.commons.Strings;
import sirius.kernel.di.std.Part;
import sirius.pasta.noodle.sandbox.NoodleSandbox;
import sirius.web.resources.Resource;
import sirius.web.resources.Resources;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.Optional;

/**
 * Provides helper methods used in templates.
 */
public class ContentHelper {

    /**
     * Contains the instance which is passed into the global context as "contentHelper".
     *
     * @deprecated Tagliatelle can now directly call the static methods of this class.
     */
    @Deprecated
    public static final ContentHelper INSTANCE = new ContentHelper();

    @Part
    private static Resources resources;

    private ContentHelper() {
    }

    /**
     * Replaces new line with &lt;br&gt; tags.
     *
     * @param content the content to parse
     * @return the value of <b>content</b> where all line breaks are replaced by &lt;br&gt; tags.
     * @see StringCleanup#nlToBr(String)
     */
    @NoodleSandbox(NoodleSandbox.Accessibility.GRANTED)
    public static String nl2br(String content) {
        return Strings.cleanup(content, StringCleanup::nlToBr);
    }

    /**
     * Removes all XML characters (&lt;, &gt;, &quot;, &#039;, &amp;) from the input and replaces it with the well known
     * replacement characters.
     *
     * @param aText the text to replace
     * @return a string which can safely output in XML or HTML. Returns an empty string if the input was <tt>null</tt>.
     * @see StringCleanup#escapeXml(String)
     */
    @Nonnull
    @NoodleSandbox(NoodleSandbox.Accessibility.GRANTED)
    public static String escapeXML(@Nullable Object aText) {
        if (Strings.isEmpty(aText)) {
            return "";
        }
        return Strings.cleanup(aText.toString(), StringCleanup::escapeXml);
    }

    /**
     * Removes all XML tags from a given content.
     *
     * @param content content to strip XML of
     * @return content without XML tags
     * @see StringCleanup#replaceXml(String)
     */
    @NoodleSandbox(NoodleSandbox.Accessibility.GRANTED)
    public String stripXML(String content) {
        return Strings.cleanup(content, StringCleanup::replaceXml);
    }

    /**
     * Returns the contents of the given template as a single line string which can be embedded into a string
     * enclosed by ' (e.g. a JavaScript string).
     *
     * @param resource the template to fetch
     * @return the contents of the template without line breaks and with escaped ticks (<tt>'</tt>). If the template
     * cannot be found an empty string is returned
     */
    @Nonnull
    @NoodleSandbox(NoodleSandbox.Accessibility.GRANTED)
    public String getResourceAsInlineString(String resource) {
        Optional<Resource> res = resources.resolve(resource);
        return res.map(r -> r.getContentAsString().replaceAll("\\r?\\n", " ").replace("'", "\\'")).orElse("");
    }
}
