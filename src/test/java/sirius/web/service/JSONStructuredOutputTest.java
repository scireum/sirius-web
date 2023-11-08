/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.service;

import com.fasterxml.jackson.databind.node.ObjectNode;
import org.junit.jupiter.api.Test;
import sirius.kernel.commons.Amount;
import sirius.kernel.commons.Json;
import sirius.web.services.JSONStructuredOutput;

import java.io.ByteArrayOutputStream;
import java.nio.charset.StandardCharsets;

import static org.junit.jupiter.api.Assertions.assertEquals;

class JSONStructuredOutputTest {

    @Test
    void writeEmptyArray() {
        ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
        JSONStructuredOutput out = new JSONStructuredOutput(byteArrayOutputStream, null, StandardCharsets.UTF_8.name());

        out.beginArray("");
        out.endArray();
        out.finalizeOutput();

        assertEquals("[]", byteArrayOutputStream.toString());
    }

    @Test
    void writeAmountWithinJacksonObject() {
        ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
        JSONStructuredOutput out = new JSONStructuredOutput(byteArrayOutputStream, null, StandardCharsets.UTF_8.name());

        ObjectNode objectNodeWithAmount = Json.createObject().putPOJO("amount", Amount.of(1.23));
        out.writeProperty(null, objectNodeWithAmount);
        out.finalizeOutput();

        assertEquals("{\"amount\":1.23}", byteArrayOutputStream.toString());
    }
}
