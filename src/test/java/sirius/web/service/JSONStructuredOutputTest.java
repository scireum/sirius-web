/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.service;

import org.junit.jupiter.api.Test;
import sirius.web.services.JSONStructuredOutput;

import java.io.ByteArrayOutputStream;
import java.nio.charset.StandardCharsets;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class JSONStructuredOutputTest {

    @Test
    public void writeEmptyArray() {
        ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
        JSONStructuredOutput out = new JSONStructuredOutput(byteArrayOutputStream, null, StandardCharsets.UTF_8.name());

        out.beginArray("");
        out.endArray();
        out.finalizeOutput();

        assertEquals("[]", byteArrayOutputStream.toString());
    }
}
