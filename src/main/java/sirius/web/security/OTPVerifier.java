/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.security;

import com.google.common.io.BaseEncoding;
import sirius.kernel.commons.Strings;
import sirius.kernel.di.std.ConfigValue;
import sirius.kernel.di.std.Register;
import sirius.kernel.health.Exceptions;

import javax.annotation.Nonnull;
import javax.crypto.Mac;
import javax.crypto.spec.SecretKeySpec;
import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import java.time.Duration;

/**
 * Utility class to support two factor authentication (aka One Time Passwords).
 * <p>
 * The idea of two factor authentication is that a valid user is authenticated by something he or she knows
 * (a password) and something he or she has (a security token or a mobile phone + app). Hence the name
 * "two factor authentication".
 * <p>
 * This app or security token generates time based codes (sometime referred to as "one time password" OTP). This
 * helper class can be used to generate a configuration QR code (which is essentially a configuration URL used by
 * popular apps (like Google Authenticator). It also permits to verify OTP submitted by the user.
 */
@Register(classes = OTPVerifier.class)
public class OTPVerifier {

    /**
     * How many intervals + and - do we check in order to compensate clock drift
     * etc.
     */
    @ConfigValue("http.otp.graceNumberOfIntervals")
    private int numberOfGraceIntervals;

    /**
     * Length of a time interval in seconds. Google Authenticator uses 30s
     */
    @ConfigValue("http.otp.timeInterval")
    private Duration timeInterval;

    /**
     * Returns a randomly generated key which can be used as shared secret.
     * <p>
     * A secret like this has to be stored per user. It is required by {@link #checkCode(String, String)} to verify
     * a given OTP. Also use {@link #getAsAuthURL(String, String)} to generate an URL which can be put into
     * a QR code to configure apps like Google Authenticator.
     *
     * @return a secret key to be used as input for <tt>checkCode</tt> and <tt>getAsAuthURL</tt>
     */
    @Nonnull
    public String generateSharedSecret() {
        byte[] buffer = new byte[10];
        new SecureRandom().nextBytes(buffer);
        return BaseEncoding.base32().encode(buffer);
    }

    /**
     * Generates an OTPAUTH-URL which can be used to generate a QR code for a
     * mobile device.
     *
     * @param account name of the account associated with this code
     * @param secret  the secret used to generate the OTP codes
     * @return a special URL used by popular apps like Google Authenticator for "automatic" configuration of the
     * account
     */
    @Nonnull
    public String getAsAuthURL(String account, String secret) {
        return "otpauth://totp/" + account.replace(" ", "_") + "?secret=" + Strings.urlEncode(secret);
    }

    /**
     * Verifies the given OTP against the given secret key.
     *
     * @param secret the secret stored for this user
     * @param code   the OTP entered by the user
     * @return <tt>true</tt> if the given OTP is currently valid, <tt>false</tt> otherwise
     */
    public boolean checkCode(String secret, String code) {
        if (Strings.isEmpty(secret)) {
            return false;
        }
        if (Strings.isEmpty(code)) {
            return false;
        }
        byte[] decodedKey = BaseEncoding.base32().decode(secret);

        // Try timebased code...
        // The used timevalue is the index of the n seconds interval of the
        // current unix time.
        long t = System.currentTimeMillis() / timeInterval.toMillis();
        // We check several codes to compensate for clock drift.
        int window = numberOfGraceIntervals;
        for (int i = -window; i <= window; ++i) {
            String hash = String.valueOf(extractOTPCode(decodedKey, t + i));
            if (Strings.areEqual(hash, code)) {
                return true;
            }
        }

        // The validation code is invalid.
        return false;
    }

    /**
     * Computes an OTP code for the given secret and current interval.
     *
     * @param secret the shared secret of the user to compute a code for.
     * @return a valid OTP code for the current time interval
     */
    @Nonnull
    public String computeCode(@Nonnull String secret) {
        byte[] decodedKey = BaseEncoding.base32().decode(secret);
        long t = System.currentTimeMillis() / timeInterval.toMillis();
        return extractOTPCode(decodedKey, t);
    }

    /*
     * An OTP code always has 6 digits...
     */
    private static final int CODE_LENGTH = 6;

    /*
     * Extracts the given code the given code by applying a HmacSHA1 on the time interval and the given secret key
     */
    private String extractOTPCode(byte[] key, long t) {
        try {
            byte[] data = new byte[8];
            long value = t;

            for (int i = 7; i >= 0; i--) {
                data[i] = (byte) value;
                value >>>= 8;
            }

            SecretKeySpec signKey = new SecretKeySpec(key, "HmacSHA1");
            Mac mac = Mac.getInstance("HmacSHA1");
            mac.init(signKey);
            byte[] hash = mac.doFinal(data);

            int offset = hash[20 - 1] & 0xF;

            // We're using a long because Java hasn't got unsigned int.
            long truncatedHash = 0;
            for (int i = 0; i < 4; ++i) {
                truncatedHash <<= 8;
                // We are dealing with signed bytes:
                // we just keep the first byte.
                truncatedHash |= hash[offset + i] & 0xFF;
            }

            truncatedHash &= 0x7FFFFFFF;
            truncatedHash %= (int) Math.pow(10, CODE_LENGTH);
            String result = String.valueOf(truncatedHash);

            while (result.length() < CODE_LENGTH) {
                result = "0" + result;
            }

            return result;
        } catch (NoSuchAlgorithmException | InvalidKeyException e) {
            throw Exceptions.handle(e);
        }
    }
}
