/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.security

import sirius.kernel.BaseSpecification
import sirius.kernel.di.std.Part
import sirius.kernel.health.HandledException

class SamlSpec extends BaseSpecification {

    @Part
    static SAMLHelper saml;

    SAMLResponse parseSAMLResponse(String response, boolean checkTime) {
        InputStream input = new ByteArrayInputStream(response.getBytes())
        try {
            return saml.parseSAMLResponse(input, checkTime);
        } finally {
            input.close();
        }
    }

    def "ADFS SAML response parses correctly when ignoring time"() {
        when:
        def response = parseSAMLResponse("<samlp:Response ID=\"_f37ff725-146f-4074-bb84-29891ac77394\" Version=\"2.0\" IssueInstant=\"2022-05-12T11:17:00.648Z\" Destination=\"https://memoio.staging.scireum.com/saml/login\" Consent=\"urn:oasis:names:tc:SAML:2.0:consent:unspecified\" InResponseTo=\"identifier_1652354198578\" xmlns:samlp=\"urn:oasis:names:tc:SAML:2.0:protocol\"><Issuer xmlns=\"urn:oasis:names:tc:SAML:2.0:assertion\">https://sso.hq.scireum.com/adfs/services/trust</Issuer><samlp:Status><samlp:StatusCode Value=\"urn:oasis:names:tc:SAML:2.0:status:Success\" /></samlp:Status><Assertion ID=\"_e1ab2d36-f043-47ea-8a1a-2c2910a5a8e9\" IssueInstant=\"2022-05-12T11:17:00.648Z\" Version=\"2.0\" xmlns=\"urn:oasis:names:tc:SAML:2.0:assertion\"><Issuer>https://sso.hq.scireum.com/adfs/services/trust</Issuer><ds:Signature xmlns:ds=\"http://www.w3.org/2000/09/xmldsig#\"><ds:SignedInfo><ds:CanonicalizationMethod Algorithm=\"http://www.w3.org/2001/10/xml-exc-c14n#\" /><ds:SignatureMethod Algorithm=\"http://www.w3.org/2001/04/xmldsig-more#rsa-sha256\" /><ds:Reference URI=\"#_e1ab2d36-f043-47ea-8a1a-2c2910a5a8e9\"><ds:Transforms><ds:Transform Algorithm=\"http://www.w3.org/2000/09/xmldsig#enveloped-signature\" /><ds:Transform Algorithm=\"http://www.w3.org/2001/10/xml-exc-c14n#\" /></ds:Transforms><ds:DigestMethod Algorithm=\"http://www.w3.org/2001/04/xmlenc#sha256\" /><ds:DigestValue>78HD8z2G9ElX+fTTHAnKYZpUTxItTZH32/4oTqAATpU=</ds:DigestValue></ds:Reference></ds:SignedInfo><ds:SignatureValue>RkLC5skfz346fLRJeRPwhjj3W6XGlEuhpcBRdhWp1vYBneKewVdNo5AAsU3fzs5n+qlNE1slr+lzsA0bw/rlYSsgJkDheCYS7Ltg5rT2utEqkJS5IlnFnkNfq3wi/dTTLXbcWEDatijk21hKOkAMfGIWK5+jq8RmkFnsPxap2zHGTrWqs2nbRGP3iykSaoYciWjSvN88RQsmoJaT/yWO1xPoGwV800x6CDNVoQQ9+9UsBabTpJWxAx2bZE1TKoD8wxf12gTH0Or53N4hNWEuD/xKYjfTI+NV3wreQsLFW/zfVGHZ7VGkX+XBmvd34k++lfiwkKcqWGzT6omFMwbi8A==</ds:SignatureValue><KeyInfo xmlns=\"http://www.w3.org/2000/09/xmldsig#\"><ds:X509Data><ds:X509Certificate>MIIC4DCCAcigAwIBAgIQScrocMfxc4FJpePGt81dKDANBgkqhkiG9w0BAQsFADAsMSowKAYDVQQDEyFBREZTIFNpZ25pbmcgLSBzc28uaHEuc2NpcmV1bS5jb20wHhcNMjEwOTEzMTMzMTM3WhcNMjQwOTEyMTMzMTM3WjAsMSowKAYDVQQDEyFBREZTIFNpZ25pbmcgLSBzc28uaHEuc2NpcmV1bS5jb20wggEiMA0GCSqGSIb3DQEBAQUAA4IBDwAwggEKAoIBAQCaC+lpjegfdnsf7H4zxuI0jxBDK5Roq6A60y4QJ1f2ysHi/7Gn4DFavdJJ4wPvPbJzzkOykXVcgn+BmKfscRvy8h6pW21dVMwwxC5q/vnvui0rqZYCHD6HmMrR2rTEEO+w5W4AznFzRl/blo4QHFtwLWGscbGaeO7m9+VUjXZ5fO9lg7rngHw5jjVPtgu8d57p88zitFbbK/DWYcWoOgj98fubU9nLyRYJmHavoL5ZEWX7hesvXEaCjeaZuJUPEF8HYrlmc1+LWGNc2uUpXZC/qcOb3eD4JXPRQKpzGW8A3+wCnr3wtlJHM6at6h7ZBpRc2nvGkb/u892isn9dsL47AgMBAAEwDQYJKoZIhvcNAQELBQADggEBAEfA2AD48pzStBtgq/yLrLMHSfDAZiBQhrAn6/1veKDU80ruz1Rd/wpjS19rPLp7kjiaRSAp8aKQDeaQDRmMNczN958ry1P2vd12Teilp1zBo0uZ9ce7tAt+eV4kyh3mvyUzL5RGSpEBJ+Cf/gVo/zH+Yy/x07urHt0POmO5zaw/ZhW7dZoWHzCzJ/fjnJ+UlaLIlgvvT0TxyspadWqVeZLgL+nnC2vy+YYESWU3auv30ykkWgpc246Hy1+zsf0z5KIQJuwkVAjY5hpMWNclgTT4QBBFWY5zGgfPiNby76Eks4ryAA889X4teC61wiHaj1Ok3CZ4Y9iLYLg2OQzONgs=</ds:X509Certificate></ds:X509Data></KeyInfo></ds:Signature><Subject><SubjectConfirmation Method=\"urn:oasis:names:tc:SAML:2.0:cm:bearer\"><SubjectConfirmationData InResponseTo=\"identifier_1652354198578\" NotOnOrAfter=\"2022-05-12T11:22:00.648Z\" Recipient=\"https://memoio.staging.scireum.com/saml/login\" /></SubjectConfirmation></Subject><Conditions NotBefore=\"2022-05-12T11:17:00.648Z\" NotOnOrAfter=\"2022-05-12T12:17:00.648Z\"><AudienceRestriction><Audience>memoio-staging</Audience></AudienceRestriction></Conditions><AttributeStatement><Attribute Name=\"http://schemas.xmlsoap.org/ws/2005/05/identity/claims/emailaddress\"><AttributeValue>jvo@scireum.de</AttributeValue></Attribute><Attribute Name=\"http://schemas.microsoft.com/ws/2008/06/identity/claims/role\"><AttributeValue>administrator,user-administrator,jobs-manager,permission-manage-company,,permission-manage-channels,,permission-manage-broadcasts,,permission-manage-files,permission-manage-custom-folders,permission-wipe-users</AttributeValue></Attribute><Attribute Name=\"http://schemas.xmlsoap.org/ws/2005/05/identity/claims/name\"><AttributeValue>Jakob Vogel</AttributeValue></Attribute></AttributeStatement><AuthnStatement AuthnInstant=\"2022-05-12T11:17:00.570Z\"><AuthnContext><AuthnContextClassRef>urn:oasis:names:tc:SAML:2.0:ac:classes:PasswordProtectedTransport</AuthnContextClassRef></AuthnContext></AuthnStatement></Assertion></samlp:Response>", false)
        then:
        response.getFingerprint() == "c5fbeb487860c8e3cbe56409617021d01be371ff"
        response.getIssuer() == "https://sso.hq.scireum.com/adfs/services/trust"
    }

    def "ADFS SAML response fails correctly when considering time"() {
        when:
        def response = parseSAMLResponse("<samlp:Response ID=\"_f37ff725-146f-4074-bb84-29891ac77394\" Version=\"2.0\" IssueInstant=\"2022-05-12T11:17:00.648Z\" Destination=\"https://memoio.staging.scireum.com/saml/login\" Consent=\"urn:oasis:names:tc:SAML:2.0:consent:unspecified\" InResponseTo=\"identifier_1652354198578\" xmlns:samlp=\"urn:oasis:names:tc:SAML:2.0:protocol\"><Issuer xmlns=\"urn:oasis:names:tc:SAML:2.0:assertion\">https://sso.hq.scireum.com/adfs/services/trust</Issuer><samlp:Status><samlp:StatusCode Value=\"urn:oasis:names:tc:SAML:2.0:status:Success\" /></samlp:Status><Assertion ID=\"_e1ab2d36-f043-47ea-8a1a-2c2910a5a8e9\" IssueInstant=\"2022-05-12T11:17:00.648Z\" Version=\"2.0\" xmlns=\"urn:oasis:names:tc:SAML:2.0:assertion\"><Issuer>https://sso.hq.scireum.com/adfs/services/trust</Issuer><ds:Signature xmlns:ds=\"http://www.w3.org/2000/09/xmldsig#\"><ds:SignedInfo><ds:CanonicalizationMethod Algorithm=\"http://www.w3.org/2001/10/xml-exc-c14n#\" /><ds:SignatureMethod Algorithm=\"http://www.w3.org/2001/04/xmldsig-more#rsa-sha256\" /><ds:Reference URI=\"#_e1ab2d36-f043-47ea-8a1a-2c2910a5a8e9\"><ds:Transforms><ds:Transform Algorithm=\"http://www.w3.org/2000/09/xmldsig#enveloped-signature\" /><ds:Transform Algorithm=\"http://www.w3.org/2001/10/xml-exc-c14n#\" /></ds:Transforms><ds:DigestMethod Algorithm=\"http://www.w3.org/2001/04/xmlenc#sha256\" /><ds:DigestValue>78HD8z2G9ElX+fTTHAnKYZpUTxItTZH32/4oTqAATpU=</ds:DigestValue></ds:Reference></ds:SignedInfo><ds:SignatureValue>RkLC5skfz346fLRJeRPwhjj3W6XGlEuhpcBRdhWp1vYBneKewVdNo5AAsU3fzs5n+qlNE1slr+lzsA0bw/rlYSsgJkDheCYS7Ltg5rT2utEqkJS5IlnFnkNfq3wi/dTTLXbcWEDatijk21hKOkAMfGIWK5+jq8RmkFnsPxap2zHGTrWqs2nbRGP3iykSaoYciWjSvN88RQsmoJaT/yWO1xPoGwV800x6CDNVoQQ9+9UsBabTpJWxAx2bZE1TKoD8wxf12gTH0Or53N4hNWEuD/xKYjfTI+NV3wreQsLFW/zfVGHZ7VGkX+XBmvd34k++lfiwkKcqWGzT6omFMwbi8A==</ds:SignatureValue><KeyInfo xmlns=\"http://www.w3.org/2000/09/xmldsig#\"><ds:X509Data><ds:X509Certificate>MIIC4DCCAcigAwIBAgIQScrocMfxc4FJpePGt81dKDANBgkqhkiG9w0BAQsFADAsMSowKAYDVQQDEyFBREZTIFNpZ25pbmcgLSBzc28uaHEuc2NpcmV1bS5jb20wHhcNMjEwOTEzMTMzMTM3WhcNMjQwOTEyMTMzMTM3WjAsMSowKAYDVQQDEyFBREZTIFNpZ25pbmcgLSBzc28uaHEuc2NpcmV1bS5jb20wggEiMA0GCSqGSIb3DQEBAQUAA4IBDwAwggEKAoIBAQCaC+lpjegfdnsf7H4zxuI0jxBDK5Roq6A60y4QJ1f2ysHi/7Gn4DFavdJJ4wPvPbJzzkOykXVcgn+BmKfscRvy8h6pW21dVMwwxC5q/vnvui0rqZYCHD6HmMrR2rTEEO+w5W4AznFzRl/blo4QHFtwLWGscbGaeO7m9+VUjXZ5fO9lg7rngHw5jjVPtgu8d57p88zitFbbK/DWYcWoOgj98fubU9nLyRYJmHavoL5ZEWX7hesvXEaCjeaZuJUPEF8HYrlmc1+LWGNc2uUpXZC/qcOb3eD4JXPRQKpzGW8A3+wCnr3wtlJHM6at6h7ZBpRc2nvGkb/u892isn9dsL47AgMBAAEwDQYJKoZIhvcNAQELBQADggEBAEfA2AD48pzStBtgq/yLrLMHSfDAZiBQhrAn6/1veKDU80ruz1Rd/wpjS19rPLp7kjiaRSAp8aKQDeaQDRmMNczN958ry1P2vd12Teilp1zBo0uZ9ce7tAt+eV4kyh3mvyUzL5RGSpEBJ+Cf/gVo/zH+Yy/x07urHt0POmO5zaw/ZhW7dZoWHzCzJ/fjnJ+UlaLIlgvvT0TxyspadWqVeZLgL+nnC2vy+YYESWU3auv30ykkWgpc246Hy1+zsf0z5KIQJuwkVAjY5hpMWNclgTT4QBBFWY5zGgfPiNby76Eks4ryAA889X4teC61wiHaj1Ok3CZ4Y9iLYLg2OQzONgs=</ds:X509Certificate></ds:X509Data></KeyInfo></ds:Signature><Subject><SubjectConfirmation Method=\"urn:oasis:names:tc:SAML:2.0:cm:bearer\"><SubjectConfirmationData InResponseTo=\"identifier_1652354198578\" NotOnOrAfter=\"2022-05-12T11:22:00.648Z\" Recipient=\"https://memoio.staging.scireum.com/saml/login\" /></SubjectConfirmation></Subject><Conditions NotBefore=\"2022-05-12T11:17:00.648Z\" NotOnOrAfter=\"2022-05-12T12:17:00.648Z\"><AudienceRestriction><Audience>memoio-staging</Audience></AudienceRestriction></Conditions><AttributeStatement><Attribute Name=\"http://schemas.xmlsoap.org/ws/2005/05/identity/claims/emailaddress\"><AttributeValue>jvo@scireum.de</AttributeValue></Attribute><Attribute Name=\"http://schemas.microsoft.com/ws/2008/06/identity/claims/role\"><AttributeValue>administrator,user-administrator,jobs-manager,permission-manage-company,,permission-manage-channels,,permission-manage-broadcasts,,permission-manage-files,permission-manage-custom-folders,permission-wipe-users</AttributeValue></Attribute><Attribute Name=\"http://schemas.xmlsoap.org/ws/2005/05/identity/claims/name\"><AttributeValue>Jakob Vogel</AttributeValue></Attribute></AttributeStatement><AuthnStatement AuthnInstant=\"2022-05-12T11:17:00.570Z\"><AuthnContext><AuthnContextClassRef>urn:oasis:names:tc:SAML:2.0:ac:classes:PasswordProtectedTransport</AuthnContextClassRef></AuthnContext></AuthnStatement></Assertion></samlp:Response>", true)
        then:
        def e = thrown(HandledException)
        e.getMessage() == "Ein Fehler ist aufgetreten: Invalid SAML Response: Invalid IssueInstant: 2022-05-12T11:17:00.648Z"
    }

    def "logind SAML response parses correctly when ignoring time"() {
        when:
        def response = parseSAMLResponse("<?xml version=\"1.0\"?>\n" +
                                                 "<samlp:Response xmlns:samlp=\"urn:oasis:names:tc:SAML:2.0:protocol\" ID=\"5c0fd610-715f-4187-8f5e-298ca16dd37d\" Version=\"2.0\" IssueInstant=\"\" Destination=\"https://memoio.test.scireum.com/saml/login\" Consent=\"urn:oasis:names:tc:SAML:2.0:consent:unspecified\" InResponseTo=\"identifier_1652353644351\"><Issuer xmlns=\"urn:oasis:names:tc:SAML:2.0:assertion\">https://sso.hq.scireum.com/adfs/services/trust</Issuer><samlp:Status><samlp:StatusCode Value=\"urn:oasis:names:tc:SAML:2.0:status:Success\"/></samlp:Status><Assertion xmlns=\"urn:oasis:names:tc:SAML:2.0:assertion\" ID=\"_30e5c8bb-ee3a-4290-9960-f5fae240d0e0\" IssueInstant=\"2022-05-12T11:07:26.759Z\" Version=\"2.0\"><Issuer>https://sso.hq.scireum.com/adfs/services/trust</Issuer><ds:Signature xmlns:ds=\"http://www.w3.org/2000/09/xmldsig#\"><ds:SignedInfo><ds:CanonicalizationMethod Algorithm=\"http://www.w3.org/2001/10/xml-exc-c14n#\"/><ds:SignatureMethod Algorithm=\"http://www.w3.org/2001/04/xmldsig-more#rsa-sha256\"/><ds:Reference URI=\"#_30e5c8bb-ee3a-4290-9960-f5fae240d0e0\"><ds:Transforms><ds:Transform Algorithm=\"http://www.w3.org/2000/09/xmldsig#enveloped-signature\"/><ds:Transform Algorithm=\"http://www.w3.org/2001/10/xml-exc-c14n#\"/></ds:Transforms><ds:DigestMethod Algorithm=\"http://www.w3.org/2001/04/xmlenc#sha256\"/><ds:DigestValue>oSkdX5u1Xt5PoFYL4HB+3iN9Y5Flmo8PJcL72gLBPr0=</ds:DigestValue></ds:Reference></ds:SignedInfo><ds:SignatureValue>wK618x44fMUudGg4xnGw8gETbVxMYGv73YMpGuD5gS8mOcU1xS9t2gninPexZEzh\n" +
                                                 "Srx68o+5c1vmUgen+Z/t94jfw35cBIOnbMkWXfs3aovSaKqUAgRcgT+c7pMYofX4\n" +
                                                 "cv0aOoBNhH+S4HL/8eGYo3yS32vk/Ngm/KdMmteeFsL/6v5cCTrgViCvhqhLVccX\n" +
                                                 "fhzy5Z2IN1SPc6fOXTN6eXTGwTz43T7kk0XOgFrx88bOdf3ffgSOaIsmYT4q9khs\n" +
                                                 "a31GTO/akfFzeJwqXTVANokZ4Y2b5e2PQw+QJTuKMUYAGKWTpHOq+23a6TQwpeTo\n" +
                                                 "XwEfZn3REAEJG1RkZc1tlA==</ds:SignatureValue><ds:KeyInfo xmlns=\"http://www.w3.org/2000/09/xmldsig#\"><ds:X509Data>\n" +
                                                 "<ds:X509Certificate>MIIDGzCCAgOgAwIBAgIUKWE56kp2noHiNxwSDkWFAfxQJAcwDQYJKoZIhvcNAQEL\n" +
                                                 "BQAwHTEbMBkGA1UEAwwSbG9naW5kLnNjaXJldW0uY29tMB4XDTIyMDUwMzE4NTgw\n" +
                                                 "OVoXDTMyMDQzMDE4NTgwOVowHTEbMBkGA1UEAwwSbG9naW5kLnNjaXJldW0uY29t\n" +
                                                 "MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAzjx3Ov933PplnBfFMrr3\n" +
                                                 "rxz11QKqqEOd2ewrCQunoxLLIJzw9RQWS8oAk8xO9FOm72RIlKU4SNa/PvBak+Ez\n" +
                                                 "hOuFwwSzVGVLg8eZm05TAN+Zf0PCe1wI8a4l/lDhtVEr03Fe+hn0Pl1c+fd/TVyR\n" +
                                                 "mo11io/ZWOISM/ILmfVlVAbrPdVcnNJ6DTC56j8OdniRK6mbKX70wjcTsnPMR4B1\n" +
                                                 "MHSfxmPjycYXq9nYaurE+5Hdsda8+JzXxWua/0B0C7OI2IHoqZ3fwsulFrey/DEh\n" +
                                                 "5NkCkyj/lbE1UU3YbetWR8C1/I6J0eFK6l5GYoofd+Zici2/dtDtIzDicVsq3EqG\n" +
                                                 "iwIDAQABo1MwUTAdBgNVHQ4EFgQUfYTw8Bft7uoqY7kgRMZ6NkR7Jg8wHwYDVR0j\n" +
                                                 "BBgwFoAUfYTw8Bft7uoqY7kgRMZ6NkR7Jg8wDwYDVR0TAQH/BAUwAwEB/zANBgkq\n" +
                                                 "hkiG9w0BAQsFAAOCAQEASLXhVm3d/IGGhDcZAijDOM2g89W+IxVGWkfpnc797xKL\n" +
                                                 "pc91m5k3E1nN0hxt/uxnfOqJr+iitdHM/+c6iBmSILygOLe+ATwNjEZ+AEJPd04k\n" +
                                                 "GNa5lupf6Fr4poJ3lEclDET9peKYAQdh71iY7e0TLg+1W/lOCvNCnmgmY9vmSTzE\n" +
                                                 "Ps0C4RbuuJagURKHTqc757OawimGZvN0dA2JJ7c6m0gjFxeQ2pX0bVOAGmVeD6FR\n" +
                                                 "UtS2kvA28X4ToQg3REfK8K+MroixIpwVfdyHRCP4CsLrz4w+EJw4VlWAzJ45HFHg\n" +
                                                 "15uMlXcnBt42Bmx7ifeCuOhqgdTHHWpMtSq23dIS/A==</ds:X509Certificate>\n" +
                                                 "</ds:X509Data></ds:KeyInfo></ds:Signature><Subject><NameID>jge@scireum.de</NameID><SubjectConfirmation Method=\"urn:oasis:names:tc:SAML:2.0:cm:bearer\"><SubjectConfirmationData InResponseTo=\"identifier_1652353644351\" NotOnOrAfter=\"2022-05-12T13:07:26.759Z\" Recipient=\"https://sellsite.test.scireum.com/saml/login\"/></SubjectConfirmation></Subject><Conditions NotBefore=\"2022-05-12T09:07:26.759Z\" NotOnOrAfter=\"2022-05-12T13:07:26.759Z\"><AudienceRestriction><Audience>sellsite-test</Audience></AudienceRestriction></Conditions><AuthnStatement AuthnInstant=\"2022-05-12T11:07:26.759Z\" SessionIndex=\"_30e5c8bb-ee3a-4290-9960-f5fae240d0e0\"><AuthnContext><AuthnContextClassRef>urn:oasis:names:tc:SAML:2.0:ac:classes:PasswordProtectedTransport</AuthnContextClassRef></AuthnContext></AuthnStatement><AttributeStatement><Attribute Name=\"http://schemas.xmlsoap.org/ws/2005/05/identity/claims/emailaddress\"><AttributeValue>jvo@scireum.de</AttributeValue></Attribute><Attribute Name=\"http://schemas.xmlsoap.org/ws/2005/05/identity/claims/surname\"><AttributeValue>Vogel</AttributeValue></Attribute><Attribute Name=\"http://schemas.xmlsoap.org/ws/2005/05/identity/claims/givenname\"><AttributeValue>Jakob</AttributeValue></Attribute><Attribute Name=\"http://schemas.microsoft.com/ws/2008/06/identity/claims/role\"><AttributeValue>administrator,user-administrator,jobs-manager,permission-manage-company,,permission-manage-channels,,permission-manage-broadcasts,,permission-manage-files,permission-manage-custom-folders,permission-wipe-users</AttributeValue><AttributeValue>permission-manage-custom-folders</AttributeValue></Attribute><Attribute Name=\"http://schemas.xmlsoap.org/ws/2005/05/identity/claims/name\"><AttributeValue>Jakob Vogel via G</AttributeValue></Attribute></AttributeStatement></Assertion></samlp:Response>\n", false)
        then:
        response.getIssuer() == "https://sso.hq.scireum.com/adfs/services/trust"
        response.getFingerprint() == "71038506714cb316a8cb6500b26551b1c29375ce"
    }

    def "cidaas SAML response parses correctly when ignoring time"() {
        when:
        def response = parseSAMLResponse(
                "<?xml version=\"1.0\"?>\n" +
                        "<samlp:Response xmlns:samlp=\"urn:oasis:names:tc:SAML:2.0:protocol\" ID=\"_e213209f-d721-4ebf-a93f-f20faeebcfb8\" InResponseTo=\"identifier_1652099895318\" Version=\"2.0\" IssueInstant=\"2022-05-09T12:38:29.862Z\" Destination=\"https://oxomi.com/saml/SAML_TEST\">\n" +
                        "    <saml:Issuer xmlns:saml=\"urn:oasis:names:tc:SAML:2.0:assertion\">urn:test.test.eu</saml:Issuer>\n" +
                        "    <samlp:Status>\n" +
                        "        <samlp:StatusCode Value=\"urn:oasis:names:tc:SAML:2.0:status:Success\"/>\n" +
                        "    </samlp:Status>\n" +
                        "    <saml:Assertion xmlns:saml=\"urn:oasis:names:tc:SAML:2.0:assertion\" ID=\"_6375e9da-8acb-4e52-8091-ecd17ab0b2d3\" Version=\"2.0\" IssueInstant=\"2022-05-09T12:38:29.862Z\">\n" +
                        "        <saml:Issuer>urn:test.test.eu</saml:Issuer>\n" +
                        "        <ds:Signature xmlns:ds=\"http://www.w3.org/2000/09/xmldsig#\">\n" +
                        "            <ds:SignedInfo>\n" +
                        "                <ds:CanonicalizationMethod Algorithm=\"http://www.w3.org/2001/10/xml-exc-c14n#\"/>\n" +
                        "                <ds:SignatureMethod Algorithm=\"http://www.w3.org/2000/09/xmldsig#rsa-sha1\"/>\n" +
                        "                <ds:Reference URI=\"#_6375e9da-8acb-4e52-8091-ecd17ab0b2d3\">\n" +
                        "                    <ds:Transforms>\n" +
                        "                        <ds:Transform Algorithm=\"http://www.w3.org/2000/09/xmldsig#enveloped-signature\"/>\n" +
                        "                        <ds:Transform Algorithm=\"http://www.w3.org/2001/10/xml-exc-c14n#\"/>\n" +
                        "                    </ds:Transforms>\n" +
                        "                    <ds:DigestMethod Algorithm=\"http://www.w3.org/2000/09/xmldsig#sha1\"/>\n" +
                        "                    <ds:DigestValue>4VHR1KwR932BxLe1Yoj3rSVLTmk=</ds:DigestValue>\n" +
                        "                </ds:Reference>\n" +
                        "            </ds:SignedInfo>\n" +
                        "            <ds:SignatureValue>dW1ed5s7hA1vH3oAT7WlmzMKERnugDKdwYc5VVO0LhTWjM6yKpaTjrRUPZ55OYzC\n" +
                        "zSOfCMVhpFLwTG+8/cul3bGELweuRK2l1vxUq0l6rOAtlQSzqyyQZSOZw9LXGmYt\n" +
                        "5jfiGvA4XTtKB/bXsdiUJ8ASHA/iVrPI9PUJBbjlaciBhKNngjCGCidczsvntWWV\n" +
                        "JxIP0HGtEzLMR2hYo+383CilhMfrlwOkjAYvC9j0yHmBkHsfLWjRACoizwI4Lze7\n" +
                        "hJybj3bH2fi4lJUCSLhPdS60nkKb5TvvM7s43ab+WV7Tlz07AIBZqcI0UFtrBbbf\n" +
                        "uw90GU7ahwpiQ4favMl7OQ==</ds:SignatureValue>\n" +
                        "            <ds:KeyInfo>\n" +
                        "                <ds:X509Data>\n" +
                        "                </ds:X509Data>\n" +
                        "            </ds:KeyInfo>\n" +
                        "        </ds:Signature>\n" +
                        "        <saml:Subject>\n" +
                        "            <saml:NameID Format=\"urn:oasis:names:tc:SAML:1.1:nameid-format:emailAddress\">foobar@example.com</saml:NameID>\n" +
                        "            <saml:SubjectConfirmation Method=\"urn:oasis:names:tc:SAML:2.0:cm:bearer\">\n" +
                        "                <saml:SubjectConfirmationData NotOnOrAfter=\"2022-05-10T12:38:29.000Z\" Recipient=\"https://oxomi.com/saml/SAML_TEST\" InResponseTo=\"identifier_1652099895318\"/>\n" +
                        "            </saml:SubjectConfirmation>\n" +
                        "        </saml:Subject>\n" +
                        "        <saml:Conditions NotBefore=\"2022-05-09T12:38:29.862Z\" NotOnOrAfter=\"2022-05-10T12:38:29.000Z\">\n" +
                        "            <saml:AudienceRestriction>\n" +
                        "                <saml:Audience>Oxomi</saml:Audience>\n" +
                        "            </saml:AudienceRestriction>\n" +
                        "        </saml:Conditions>\n" +
                        "        <saml:AuthnStatement AuthnInstant=\"2022-05-09T12:38:29.862Z\" SessionNotOnOrAfter=\"2022-05-10T12:38:29.000Z\" SessionIndex=\"_42f314cc-1bc1-42f0-adf6-57fe636d0e7c\">\n" +
                        "            <saml:AuthnContext>\n" +
                        "                <saml:AuthnContextClassRef>urn:oasis:names:tc:SAML:2.0:ac:classes:unspecified</saml:AuthnContextClassRef>\n" +
                        "            </saml:AuthnContext>\n" +
                        "        </saml:AuthnStatement>\n" +
                        "        <saml:AttributeStatement xmlns:xs=\"http://www.w3.org/2001/XMLSchema\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">\n" +
                        "            <saml:Attribute NameFormat=\"urn:oasis:names:tc:SAML:2.0:attrname-format:basic\" Name=\"given_name\">\n" +
                        "                <saml:AttributeValue xsi:type=\"xs:string\">Hubert</saml:AttributeValue>\n" +
                        "\n" +
                        "            </saml:Attribute>\n" +
                        "            <saml:Attribute NameFormat=\"urn:oasis:names:tc:SAML:2.0:attrname-format:basic\" Name=\"family_name\">\n" +
                        "                <saml:AttributeValue xsi:type=\"xs:string\">Mayer</saml:AttributeValue>\n" +
                        "\n" +
                        "            </saml:Attribute>\n" +
                        "            <saml:Attribute NameFormat=\"urn:oasis:names:tc:SAML:2.0:attrname-format:basic\" Name=\"email_verified\">\n" +
                        "                <saml:AttributeValue xsi:type=\"xs:boolean\">true</saml:AttributeValue>\n" +
                        "\n" +
                        "            </saml:Attribute>\n" +
                        "            <saml:Attribute NameFormat=\"urn:oasis:names:tc:SAML:2.0:attrname-format:basic\" Name=\"email\">\n" +
                        "                <saml:AttributeValue xsi:type=\"xs:string\">foobar@example.com</saml:AttributeValue>\n" +
                        "\n" +
                        "            </saml:Attribute>\n" +
                        "            <saml:Attribute NameFormat=\"urn:oasis:names:tc:SAML:2.0:attrname-format:basic\" Name=\"identity_id\">\n" +
                        "                <saml:AttributeValue xsi:type=\"xs:string\">5dc6bbbb-ea4f-45e2-8138-07f0cf31c300</saml:AttributeValue>\n" +
                        "\n" +
                        "            </saml:Attribute>\n" +
                        "            <saml:Attribute NameFormat=\"urn:oasis:names:tc:SAML:2.0:attrname-format:basic\" Name=\"mobile_number\">\n" +
                        "                <saml:AttributeValue xsi:type=\"xs:string\">+497344234324324</saml:AttributeValue>\n" +
                        "\n" +
                        "            </saml:Attribute>\n" +
                        "            <saml:Attribute NameFormat=\"urn:oasis:names:tc:SAML:2.0:attrname-format:basic\" Name=\"mobile_number_verified\">\n" +
                        "                <saml:AttributeValue xsi:type=\"xs:boolean\">true</saml:AttributeValue>\n" +
                        "\n" +
                        "            </saml:Attribute>\n" +
                        "            <saml:Attribute NameFormat=\"urn:oasis:names:tc:SAML:2.0:attrname-format:basic\" Name=\"provider\">\n" +
                        "                <saml:AttributeValue xsi:type=\"xs:string\">self</saml:AttributeValue>\n" +
                        "\n" +
                        "            </saml:Attribute>\n" +
                        "            <saml:Attribute NameFormat=\"urn:oasis:names:tc:SAML:2.0:attrname-format:basic\" Name=\"sub\">\n" +
                        "                <saml:AttributeValue xsi:type=\"xs:string\">8c3f185d-18eb-4b4c-a252-da73e2289402</saml:AttributeValue>\n" +
                        "\n" +
                        "            </saml:Attribute>\n" +
                        "            <saml:Attribute NameFormat=\"urn:oasis:names:tc:SAML:2.0:attrname-format:basic\" Name=\"updated_at\">\n" +
                        "                <saml:AttributeValue xsi:type=\"xs:anyType\">Tue Apr 19 2022 09:16:09 GMT+0000 (Coordinated Universal Time)</saml:AttributeValue>\n" +
                        "\n" +
                        "            </saml:Attribute>\n" +
                        "            <saml:Attribute NameFormat=\"urn:oasis:names:tc:SAML:2.0:attrname-format:basic\" Name=\"roles\">\n" +
                        "                <saml:AttributeValue xsi:type=\"xs:string\">USER</saml:AttributeValue>\n" +
                        "                <saml:AttributeValue xsi:type=\"xs:string\">APP_DEVELOPER</saml:AttributeValue>\n" +
                        "                <saml:AttributeValue xsi:type=\"xs:string\">mee:test</saml:AttributeValue>\n" +
                        "                <saml:AttributeValue xsi:type=\"xs:string\">mee:portal:admin</saml:AttributeValue>\n" +
                        "\n" +
                        "            </saml:Attribute>\n" +
                        "            <saml:Attribute NameFormat=\"urn:oasis:names:tc:SAML:2.0:attrname-format:basic\" Name=\"member_of\">\n" +
                        "                <saml:AttributeValue xsi:type=\"xs:string\">TEST_ADMINS</saml:AttributeValue>\n" +
                        "\n" +
                        "            </saml:Attribute>\n" +
                        "            <saml:Attribute NameFormat=\"urn:oasis:names:tc:SAML:2.0:attrname-format:basic\" Name=\"groups\">\n" +
                        "\n" +
                        "                <saml:AttributeValue xsi:type=\"xs:anyType\">\n" +
                        "                    <groupId>\n" +
                        "                        <saml:AttributeValue xsi:type=\"xs:string\">TEST_ADMINS</saml:AttributeValue>\n" +
                        "                    </groupId>\n" +
                        "                    <roles>\n" +
                        "                        <saml:AttributeValue xsi:type=\"xs:string\">SECONDARY_ADMIN</saml:AttributeValue>\n" +
                        "                    </roles>\n" +
                        "                </saml:AttributeValue>\n" +
                        "            </saml:Attribute>\n" +
                        "        </saml:AttributeStatement>\n" +
                        "    </saml:Assertion>\n" +
                        "</samlp:Response>\n", false)
        then:
        def e = thrown(HandledException)
        e.getMessage() == "Ein Fehler ist aufgetreten: An error occurred while parsing a SAML Response: It is forbidden to use algorithm http://www.w3.org/2000/09/xmldsig#rsa-sha1 when secure validation is enabled (javax.xml.crypto.MarshalException)"
    }
}
