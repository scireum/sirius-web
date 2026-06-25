/*
 * Made with all the love in the world
 * by scireum in Stuttgart, Germany
 *
 * Copyright by scireum GmbH
 * https://www.scireum.de - info@scireum.de
 */

package sirius.web.http;

import sirius.kernel.commons.Hasher;
import sirius.kernel.commons.Strings;
import sirius.kernel.health.Exceptions;

import javax.annotation.Nullable;
import javax.crypto.Cipher;
import javax.crypto.spec.GCMParameterSpec;
import javax.crypto.spec.SecretKeySpec;
import java.nio.charset.StandardCharsets;
import java.security.SecureRandom;
import java.util.Arrays;
import java.util.Base64;

/**
 * Encrypts and decrypts the payload stored within the client session cookie.
 * <p>
 * The client session is stored on the client side within a cookie. In order to keep its contents confidential
 * (and not just tamper-proof), the payload is encrypted using <b>AES-256 in GCM mode</b>. GCM is an authenticated
 * encryption mode and therefore also guarantees the integrity of the encrypted data.
 * <p>
 * The symmetric key is derived from the configured session secret (see {@code http.sessionSecret}) by hashing it
 * with SHA-256. A fresh random initialization vector (IV) is generated for every encryption and prepended to the
 * cipher text so that encrypting the same payload twice yields different cookies.
 * <p>
 * Encrypted values are marked with {@link #ENCRYPTION_PREFIX} so that the reading side can distinguish them from
 * legacy (unencrypted) cookies and transparently support both formats during a migration period.
 *
 * @see ClientSessionSecrets for the management of the secrets used here
 */
class ClientSessionCrypto {

    /**
     * Marks a cookie value as being encrypted using version 1 of this scheme.
     * <p>
     * Legacy cookies start with a hex encoded SHA-512 hash and therefore never collide with this prefix.
     */
    static final String ENCRYPTION_PREFIX = "E1:";

    private static final String KEY_ALGORITHM = "AES";
    private static final String CIPHER_TRANSFORMATION = "AES/GCM/NoPadding";
    private static final int GCM_IV_LENGTH = 12;
    private static final int GCM_TAG_LENGTH_BITS = 128;

    private static final SecureRandom SECURE_RANDOM = new SecureRandom();

    private ClientSessionCrypto() {
    }

    /**
     * Determines if the given cookie value is an encrypted payload created by {@link #encrypt(String, String)}.
     *
     * @param cookieValue the raw cookie value to inspect
     * @return <tt>true</tt> if the value is encrypted, <tt>false</tt> if it is a legacy plain text value
     */
    static boolean isEncrypted(@Nullable String cookieValue) {
        return cookieValue != null && cookieValue.startsWith(ENCRYPTION_PREFIX);
    }

    /**
     * Encrypts the given plain text using a key derived from the given secret.
     *
     * @param plainText the payload to encrypt
     * @param secret    the secret used to derive the symmetric key
     * @return the encrypted payload, marked with {@link #ENCRYPTION_PREFIX} and Base64 encoded
     */
    static String encrypt(String plainText, String secret) {
        try {
            byte[] iv = new byte[GCM_IV_LENGTH];
            SECURE_RANDOM.nextBytes(iv);

            Cipher cipher = Cipher.getInstance(CIPHER_TRANSFORMATION);
            cipher.init(Cipher.ENCRYPT_MODE, deriveKey(secret), new GCMParameterSpec(GCM_TAG_LENGTH_BITS, iv));
            byte[] cipherText = cipher.doFinal(plainText.getBytes(StandardCharsets.UTF_8));

            byte[] combined = new byte[iv.length + cipherText.length];
            System.arraycopy(iv, 0, combined, 0, iv.length);
            System.arraycopy(cipherText, 0, combined, iv.length, cipherText.length);

            return ENCRYPTION_PREFIX + Base64.getEncoder().encodeToString(combined);
        } catch (Exception exception) {
            throw Exceptions.handle()
                            .to(WebServer.LOG)
                            .error(exception)
                            .withSystemErrorMessage("Failed to encrypt the client session: %s (%s)")
                            .handle();
        }
    }

    /**
     * Decrypts a payload previously created by {@link #encrypt(String, String)}.
     * <p>
     * If the value cannot be decrypted (e.g. because it has been tampered with or was encrypted using a different
     * secret), <tt>null</tt> is returned so that the caller can reset the session instead of failing the request.
     *
     * @param encryptedValue the encrypted payload, including the {@link #ENCRYPTION_PREFIX}
     * @param secret         the secret used to derive the symmetric key
     * @return the decrypted plain text or <tt>null</tt> if decryption failed
     */
    @Nullable
    static String decrypt(String encryptedValue, String secret) {
        if (!isEncrypted(encryptedValue)) {
            return null;
        }
        try {
            byte[] combined = Base64.getDecoder().decode(encryptedValue.substring(ENCRYPTION_PREFIX.length()));
            if (combined.length <= GCM_IV_LENGTH) {
                return null;
            }

            byte[] iv = Arrays.copyOfRange(combined, 0, GCM_IV_LENGTH);
            byte[] cipherText = Arrays.copyOfRange(combined, GCM_IV_LENGTH, combined.length);

            Cipher cipher = Cipher.getInstance(CIPHER_TRANSFORMATION);
            cipher.init(Cipher.DECRYPT_MODE, deriveKey(secret), new GCMParameterSpec(GCM_TAG_LENGTH_BITS, iv));
            return new String(cipher.doFinal(cipherText), StandardCharsets.UTF_8);
        } catch (Exception exception) {
            // A failed decryption (wrong secret, tampered data, ...) must not break the request. The caller
            // treats a null result like an invalid integrity hash and simply resets the session.
            return null;
        }
    }

    /**
     * Derives a 256 bit AES key from the given secret by hashing it with SHA-256.
     */
    private static SecretKeySpec deriveKey(String secret) {
        byte[] key = Hasher.sha256().hash(Strings.isEmpty(secret) ? "" : secret).toHash();
        return new SecretKeySpec(key, KEY_ALGORITHM);
    }
}
