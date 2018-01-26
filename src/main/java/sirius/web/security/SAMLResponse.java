/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.security;

import sirius.kernel.commons.MultiMap;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.Collection;

public class SAMLResponse {

    private String issuer;
    private String fingerprint;
    private String nameId;
    private MultiMap<String, String> attributes;

    public static final String ATTRIBUTE_GROUP = "http://schemas.xmlsoap.org/claims/Group";
    public static final String ATTRIBUTE_GIVEN_NAME = "http://schemas.xmlsoap.org/ws/2005/05/identity/claims/givenname";
    public static final String ATTRIBUTE_SURNAME = "http://schemas.xmlsoap.org/ws/2005/05/identity/claims/surname";
    public static final String ATTRIBUTE_EMAIL_ADDRESS = "http://schemas.xmlsoap.org/ws/2005/05/identity/claims/emailaddress";

    public SAMLResponse(String issuer, String fingerprint, String nameId, MultiMap<String, String> attributes) {
        this.issuer = issuer;
        this.fingerprint = fingerprint;
        this.nameId = nameId;
        this.attributes = attributes;
    }

    public String getIssuer() {
        return issuer;
    }

    public String getFingerprint() {
        return fingerprint;
    }

    public String getNameId() {
        return nameId;
    }

    @Nonnull
    public Collection<String> getAttribute(String name) {
        return attributes.get(name);
    }

    @Nullable
    public String getAttributeValue(String name) {
        return attributes.get(name).stream().findFirst().orElse(null);
    }
}
