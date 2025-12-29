/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.security.saml;

import sirius.kernel.commons.Strings;
import sirius.kernel.di.std.Part;
import sirius.web.mails.Mails;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.stream.Collectors;

/**
 * Represents a SAML user hint which can be used to pre-fill the login screen of the Identity Provider.
 *
 * @param format the format of the hint, usually a URN as defined by SAML specifications
 * @param value  the value of the hint
 */
public record SamlUserHint(String format, String value) {

    /**
     * Defines the {@linkplain #format} for hints of unspecified nature.
     */
    public static final String FORMAT_UNSPECIFIED = "urn:oasis:names:tc:SAML:1.1:nameid-format:unspecified";

    /**
     * Defines the {@linkplain #format} for e-mail address hints.
     */
    public static final String FORMAT_EMAIL = "urn:oasis:names:tc:SAML:1.1:nameid-format:emailAddress";

    @Part
    private static Mails mails;

    /**
     * Creates a new SAML user hint, making sure that only supported formats are used and that the value is non-empty.
     *
     * @param format the format of the hint
     * @param value  the value of the hint
     */
    public SamlUserHint {
        if (Strings.isEmpty(value)) {
            throw new IllegalArgumentException("Value must not be empty");
        }
        if (!FORMAT_UNSPECIFIED.equals(format) && !FORMAT_EMAIL.equals(format)) {
            throw new IllegalArgumentException("Unsupported format: " + format);
        }
    }

    /**
     * Creates a new hint with {@linkplain #FORMAT_EMAIL email address format}.
     *
     * @param email the email address to use as hint
     * @return a new hint with email address format, holding the given email address
     */
    public static SamlUserHint withEmailAddress(String email) {
        if (Strings.isEmpty(email) || !mails.isValidMailAddress(email, null)) {
            throw new IllegalArgumentException("Invalid email address: " + email);
        }
        return new SamlUserHint(FORMAT_EMAIL, email);
    }

    /**
     * Creates a new hint by extracting the user part from the given email address and setting it as a value of
     * {@linkplain #FORMAT_UNSPECIFIED unspecified format}. If the list of known domains is not empty, the domain
     * part of the email address is checked against it. If it does not match, the entire email address is used as value.
     *
     * @param email        the email address to extract the user from
     * @param knownDomains a list of known domains to check against
     * @return a new hint with unspecified format, holding the extracted user part or the entire email address
     */
    public static SamlUserHint withUserExtractedFromEmailAddress(String email, String... knownDomains) {
        return withUserExtractedFromEmailAddress(email,
                                                 knownDomains != null ?
                                                 Arrays.asList(knownDomains) :
                                                 Collections.emptyList());
    }

    /**
     * Creates a new hint by extracting the user part from the given email address and setting it as a value of
     * {@linkplain #FORMAT_UNSPECIFIED unspecified format}. If the list of known domains is not empty, the domain
     * part of the email address is checked against it. If it does not match, the entire email address is used as value.
     *
     * @param email        the email address to extract the user from
     * @param knownDomains a list of known domains to check against
     * @return a new hint with unspecified format, holding the extracted user part or the entire email address
     */
    public static SamlUserHint withUserExtractedFromEmailAddress(String email, Collection<String> knownDomains) {
        if (Strings.isEmpty(email) || !mails.isValidMailAddress(email, null)) {
            throw new IllegalArgumentException("Invalid email address: " + email);
        }

        int atIndex = email.lastIndexOf('@');
        if (atIndex < 0) {
            return withEmailAddress(email);
        }

        String domainPart = email.substring(atIndex + 1);
        if (knownDomains != null && !knownDomains.isEmpty() && !knownDomains.stream()
                                                                            .map(String::toLowerCase)
                                                                            .collect(Collectors.toSet())
                                                                            .contains(domainPart.toLowerCase())) {
            return withEmailAddress(email);
        }

        String userPart = email.substring(0, atIndex);
        if (Strings.isEmpty(userPart)) {
            return withEmailAddress(email);
        }

        return withUnspecifiedFormat(userPart);
    }

    /**
     * Creates a new hint with {@linkplain #FORMAT_UNSPECIFIED unspecified format}.
     *
     * @param value the value to use as hint
     * @return a new hint with unspecified format, holding the given value
     */
    public static SamlUserHint withUnspecifiedFormat(String value) {
        return new SamlUserHint(FORMAT_UNSPECIFIED, value);
    }
}
