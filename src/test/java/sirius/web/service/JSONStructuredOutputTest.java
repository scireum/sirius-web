/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.service;

import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.fasterxml.jackson.databind.node.POJONode;
import org.junit.jupiter.api.Test;
import sirius.kernel.commons.Amount;
import sirius.kernel.commons.Json;
import sirius.web.services.JSONStructuredOutput;

import java.io.ByteArrayOutputStream;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.util.List;

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
    void writeAmountsWithinJacksonObject() {
        ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
        JSONStructuredOutput out = new JSONStructuredOutput(byteArrayOutputStream, null, StandardCharsets.UTF_8.name());

        ObjectNode objectNodeWithAmount = Json.createObject();
        objectNodeWithAmount.putPOJO("amountDefault", Amount.of(1.23456));
        objectNodeWithAmount.putPOJO("amountRounded", Amount.ofRounded(BigDecimal.valueOf(1.23456)));
        objectNodeWithAmount.putPOJO("amountNothing", Amount.NOTHING);
        out.writeProperty(null, objectNodeWithAmount);
        out.finalizeOutput();

        assertEquals("""
                     {"amountDefault":1.23,"amountRounded":1.23456,"amountNothing":null}""",
                     byteArrayOutputStream.toString());
    }

    @Test
    void writeJacksonArrayWithObjects() {
        ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
        JSONStructuredOutput out = new JSONStructuredOutput(byteArrayOutputStream, null, StandardCharsets.UTF_8.name());

        ArrayNode array = Json.createArray(List.of(new AnObject("a", 1), new AnObject("b", 2)));
        out.writeProperty(null, array);
        out.finalizeOutput();

        assertEquals("""
                     [{"field1":"a","field2":1},{"field1":"b","field2":2}]""", byteArrayOutputStream.toString());
    }

    @Test
    void writeJacksonPojoWithObject() {
        ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
        JSONStructuredOutput out = new JSONStructuredOutput(byteArrayOutputStream, null, StandardCharsets.UTF_8.name());

        out.writeProperty(null, new POJONode(new AnObject("a", 1)));
        out.finalizeOutput();

        assertEquals("""
                     {"field1":"a","field2":1}""", byteArrayOutputStream.toString());
    }

    private static class AnObject {
        private final String field1;
        private final int field2;

        public AnObject(String field1, int field2) {
            this.field1 = field1;
            this.field2 = field2;
        }

        public String getField1() {
            return field1;
        }

        public int getField2() {
            return field2;
        }
    }
}
