/*
 * Made with all the love in the world
 * by scireum in Stuttgart, Germany
 *
 * Copyright by scireum GmbH
 * https://www.scireum.de - info@scireum.de
 */

package sirius.web.http;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Provides tests for the {@link ClientSessionCrypto} used to protect the client session cookie.
 */
class ClientSessionCryptoTest {

    private static final String SECRET = "a-reasonably-long-and-random-session-secret";
    private static final String PAYLOAD = "abc123:?user=42&role=admin&name=John+Doe";

    @Test
    void encryptDecryptRoundTrip() {
        String encrypted = ClientSessionCrypto.encrypt(PAYLOAD, SECRET);

        assertTrue(ClientSessionCrypto.isEncrypted(encrypted));
        assertFalse(encrypted.contains("user=42"));
        assertEquals(PAYLOAD, ClientSessionCrypto.decrypt(encrypted, SECRET));
    }

    @Test
    void encryptionUsesRandomIv() {
        // Encrypting the same payload twice must yield different cookies (random IV) but decrypt to the same value.
        String first = ClientSessionCrypto.encrypt(PAYLOAD, SECRET);
        String second = ClientSessionCrypto.encrypt(PAYLOAD, SECRET);

        assertNotEquals(first, second);
        assertEquals(PAYLOAD, ClientSessionCrypto.decrypt(first, SECRET));
        assertEquals(PAYLOAD, ClientSessionCrypto.decrypt(second, SECRET));
    }

    @Test
    void decryptWithWrongSecretFails() {
        String encrypted = ClientSessionCrypto.encrypt(PAYLOAD, SECRET);

        assertNull(ClientSessionCrypto.decrypt(encrypted, "a-different-secret"));
    }

    @Test
    void decryptDetectsTampering() {
        String encrypted = ClientSessionCrypto.encrypt(PAYLOAD, SECRET);

        // Flip a character within the Base64 cipher text - GCM authentication must reject it.
        int lastIndex = encrypted.length() - 1;
        char tamperedChar = encrypted.charAt(lastIndex) == 'A' ? 'B' : 'A';
        String tampered = encrypted.substring(0, lastIndex) + tamperedChar;

        assertNull(ClientSessionCrypto.decrypt(tampered, SECRET));
    }

    @Test
    void decryptRejectsNonEncryptedValues() {
        assertFalse(ClientSessionCrypto.isEncrypted(PAYLOAD));
        assertNull(ClientSessionCrypto.decrypt(PAYLOAD, SECRET));
        assertNull(ClientSessionCrypto.decrypt("E1:not-valid-base64-@@@", SECRET));
    }
}
