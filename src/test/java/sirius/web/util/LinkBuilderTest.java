/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.util;

import org.junit.Test;

import static org.junit.Assert.assertEquals;

/**
 * Provides some basic tests for the {@link LinkBuilder}.
 */
public class LinkBuilderTest {

    @Test
    public void linkBuilderProperlyAppendsToLinks() {
        assertEquals("/link?param=value", new LinkBuilder("/link").append("param", "value").toString());
        assertEquals("/link?preset=x&param=value",
                     new LinkBuilder("/link?preset=x").append("param", "value").toString());
        assertEquals("?param=value", new LinkBuilder("").append("param", "value").toString());
    }

    @Test
    public void appendEscapesProperly() {
        assertEquals("/link?param=a+b%26c", new LinkBuilder("/link").append("param", "a b&c").toString());
        assertEquals("/link?param=", new LinkBuilder("/link").append("param", "").toString());
        assertEquals("/link?param=", new LinkBuilder("/link").append("param", null).toString());
    }

    @Test
    public void appendRawTransfersEverything() {
        assertEquals("/link?param=a b", new LinkBuilder("/link").appendRaw("param", "a b").toString());
        assertEquals("/link?param=", new LinkBuilder("/link").appendRaw("param", "").toString());
        assertEquals("/link?param=null", new LinkBuilder("/link").appendRaw("param", null).toString());
    }

    @Test
    public void appendIfFilledFiltersCorrectly() {
        assertEquals("/link?param=a", new LinkBuilder("/link").appendIfFilled("param", "a").toString());
        assertEquals("/link", new LinkBuilder("/link").appendIfFilled("param", "").toString());
        assertEquals("/link", new LinkBuilder("/link").appendIfFilled("param", null).toString());
    }
}
