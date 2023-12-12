/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.security

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.assertThrows
import org.junit.jupiter.api.extension.ExtendWith
import sirius.kernel.SiriusExtension
import sirius.kernel.health.HandledException
import kotlin.test.assertEquals
import kotlin.test.assertTrue

@ExtendWith(SiriusExtension::class)
class SamlTest {

    companion object {
        private val saml = SAMLHelper()
        fun parseSAMLResponse(response: String, checkTime: Boolean): SAMLResponse {
            response.byteInputStream().use {
                return saml.parseSAMLResponse(it, checkTime)
            }
        }
    }

    @Test
    fun `ADFS SAML response parses correctly when ignoring time`() {
        val response = parseSAMLResponse(
                """<samlp:Response ID="_f37ff725-146f-4074-bb84-29891ac77394" Version="2.0" IssueInstant="2022-05-12T11:17:00.648Z" Destination="https://memoio.staging.scireum.com/saml/login" Consent="urn:oasis:names:tc:SAML:2.0:consent:unspecified" InResponseTo="identifier_1652354198578" xmlns:samlp="urn:oasis:names:tc:SAML:2.0:protocol"><Issuer xmlns="urn:oasis:names:tc:SAML:2.0:assertion">https://sso.hq.scireum.com/adfs/services/trust</Issuer><samlp:Status><samlp:StatusCode Value="urn:oasis:names:tc:SAML:2.0:status:Success" /></samlp:Status><Assertion ID="_e1ab2d36-f043-47ea-8a1a-2c2910a5a8e9" IssueInstant="2022-05-12T11:17:00.648Z" Version="2.0" xmlns="urn:oasis:names:tc:SAML:2.0:assertion"><Issuer>https://sso.hq.scireum.com/adfs/services/trust</Issuer><ds:Signature xmlns:ds="http://www.w3.org/2000/09/xmldsig#"><ds:SignedInfo><ds:CanonicalizationMethod Algorithm="http://www.w3.org/2001/10/xml-exc-c14n#" /><ds:SignatureMethod Algorithm="http://www.w3.org/2001/04/xmldsig-more#rsa-sha256" /><ds:Reference URI="#_e1ab2d36-f043-47ea-8a1a-2c2910a5a8e9"><ds:Transforms><ds:Transform Algorithm="http://www.w3.org/2000/09/xmldsig#enveloped-signature" /><ds:Transform Algorithm="http://www.w3.org/2001/10/xml-exc-c14n#" /></ds:Transforms><ds:DigestMethod Algorithm="http://www.w3.org/2001/04/xmlenc#sha256" /><ds:DigestValue>78HD8z2G9ElX+fTTHAnKYZpUTxItTZH32/4oTqAATpU=</ds:DigestValue></ds:Reference></ds:SignedInfo><ds:SignatureValue>RkLC5skfz346fLRJeRPwhjj3W6XGlEuhpcBRdhWp1vYBneKewVdNo5AAsU3fzs5n+qlNE1slr+lzsA0bw/rlYSsgJkDheCYS7Ltg5rT2utEqkJS5IlnFnkNfq3wi/dTTLXbcWEDatijk21hKOkAMfGIWK5+jq8RmkFnsPxap2zHGTrWqs2nbRGP3iykSaoYciWjSvN88RQsmoJaT/yWO1xPoGwV800x6CDNVoQQ9+9UsBabTpJWxAx2bZE1TKoD8wxf12gTH0Or53N4hNWEuD/xKYjfTI+NV3wreQsLFW/zfVGHZ7VGkX+XBmvd34k++lfiwkKcqWGzT6omFMwbi8A==</ds:SignatureValue><KeyInfo xmlns="http://www.w3.org/2000/09/xmldsig#"><ds:X509Data><ds:X509Certificate>MIIC4DCCAcigAwIBAgIQScrocMfxc4FJpePGt81dKDANBgkqhkiG9w0BAQsFADAsMSowKAYDVQQDEyFBREZTIFNpZ25pbmcgLSBzc28uaHEuc2NpcmV1bS5jb20wHhcNMjEwOTEzMTMzMTM3WhcNMjQwOTEyMTMzMTM3WjAsMSowKAYDVQQDEyFBREZTIFNpZ25pbmcgLSBzc28uaHEuc2NpcmV1bS5jb20wggEiMA0GCSqGSIb3DQEBAQUAA4IBDwAwggEKAoIBAQCaC+lpjegfdnsf7H4zxuI0jxBDK5Roq6A60y4QJ1f2ysHi/7Gn4DFavdJJ4wPvPbJzzkOykXVcgn+BmKfscRvy8h6pW21dVMwwxC5q/vnvui0rqZYCHD6HmMrR2rTEEO+w5W4AznFzRl/blo4QHFtwLWGscbGaeO7m9+VUjXZ5fO9lg7rngHw5jjVPtgu8d57p88zitFbbK/DWYcWoOgj98fubU9nLyRYJmHavoL5ZEWX7hesvXEaCjeaZuJUPEF8HYrlmc1+LWGNc2uUpXZC/qcOb3eD4JXPRQKpzGW8A3+wCnr3wtlJHM6at6h7ZBpRc2nvGkb/u892isn9dsL47AgMBAAEwDQYJKoZIhvcNAQELBQADggEBAEfA2AD48pzStBtgq/yLrLMHSfDAZiBQhrAn6/1veKDU80ruz1Rd/wpjS19rPLp7kjiaRSAp8aKQDeaQDRmMNczN958ry1P2vd12Teilp1zBo0uZ9ce7tAt+eV4kyh3mvyUzL5RGSpEBJ+Cf/gVo/zH+Yy/x07urHt0POmO5zaw/ZhW7dZoWHzCzJ/fjnJ+UlaLIlgvvT0TxyspadWqVeZLgL+nnC2vy+YYESWU3auv30ykkWgpc246Hy1+zsf0z5KIQJuwkVAjY5hpMWNclgTT4QBBFWY5zGgfPiNby76Eks4ryAA889X4teC61wiHaj1Ok3CZ4Y9iLYLg2OQzONgs=</ds:X509Certificate></ds:X509Data></KeyInfo></ds:Signature><Subject><SubjectConfirmation Method="urn:oasis:names:tc:SAML:2.0:cm:bearer"><SubjectConfirmationData InResponseTo="identifier_1652354198578" NotOnOrAfter="2022-05-12T11:22:00.648Z" Recipient="https://memoio.staging.scireum.com/saml/login" /></SubjectConfirmation></Subject><Conditions NotBefore="2022-05-12T11:17:00.648Z" NotOnOrAfter="2022-05-12T12:17:00.648Z"><AudienceRestriction><Audience>memoio-staging</Audience></AudienceRestriction></Conditions><AttributeStatement><Attribute Name="http://schemas.xmlsoap.org/ws/2005/05/identity/claims/emailaddress"><AttributeValue>jvo@scireum.de</AttributeValue></Attribute><Attribute Name="http://schemas.microsoft.com/ws/2008/06/identity/claims/role"><AttributeValue>administrator,user-administrator,jobs-manager,permission-manage-company,,permission-manage-channels,,permission-manage-broadcasts,,permission-manage-files,permission-manage-custom-folders,permission-wipe-users</AttributeValue></Attribute><Attribute Name="http://schemas.xmlsoap.org/ws/2005/05/identity/claims/name"><AttributeValue>Jakob Vogel</AttributeValue></Attribute></AttributeStatement><AuthnStatement AuthnInstant="2022-05-12T11:17:00.570Z"><AuthnContext><AuthnContextClassRef>urn:oasis:names:tc:SAML:2.0:ac:classes:PasswordProtectedTransport</AuthnContextClassRef></AuthnContext></AuthnStatement></Assertion></samlp:Response>""",
                false
        )
        assertEquals(
                "c5fbeb487860c8e3cbe56409617021d01be371ff",
                response.fingerprint
        )
        assertEquals(
                "https://sso.hq.scireum.com/adfs/services/trust",
                response.issuer
        )
    }

    @Test
    fun `ADFS SAML response fails correctly when considering time`() {
        val exception = assertThrows<HandledException> {
            parseSAMLResponse(
                    "<samlp:Response ID=\"_f37ff725-146f-4074-bb84-29891ac77394\" Version=\"2.0\" IssueInstant=\"2022-05-12T11:17:00.648Z\" Destination=\"https://memoio.staging.scireum.com/saml/login\" Consent=\"urn:oasis:names:tc:SAML:2.0:consent:unspecified\" InResponseTo=\"identifier_1652354198578\" xmlns:samlp=\"urn:oasis:names:tc:SAML:2.0:protocol\"><Issuer xmlns=\"urn:oasis:names:tc:SAML:2.0:assertion\">https://sso.hq.scireum.com/adfs/services/trust</Issuer><samlp:Status><samlp:StatusCode Value=\"urn:oasis:names:tc:SAML:2.0:status:Success\" /></samlp:Status><Assertion ID=\"_e1ab2d36-f043-47ea-8a1a-2c2910a5a8e9\" IssueInstant=\"2022-05-12T11:17:00.648Z\" Version=\"2.0\" xmlns=\"urn:oasis:names:tc:SAML:2.0:assertion\"><Issuer>https://sso.hq.scireum.com/adfs/services/trust</Issuer><ds:Signature xmlns:ds=\"http://www.w3.org/2000/09/xmldsig#\"><ds:SignedInfo><ds:CanonicalizationMethod Algorithm=\"http://www.w3.org/2001/10/xml-exc-c14n#\" /><ds:SignatureMethod Algorithm=\"http://www.w3.org/2001/04/xmldsig-more#rsa-sha256\" /><ds:Reference URI=\"#_e1ab2d36-f043-47ea-8a1a-2c2910a5a8e9\"><ds:Transforms><ds:Transform Algorithm=\"http://www.w3.org/2000/09/xmldsig#enveloped-signature\" /><ds:Transform Algorithm=\"http://www.w3.org/2001/10/xml-exc-c14n#\" /></ds:Transforms><ds:DigestMethod Algorithm=\"http://www.w3.org/2001/04/xmlenc#sha256\" /><ds:DigestValue>78HD8z2G9ElX+fTTHAnKYZpUTxItTZH32/4oTqAATpU=</ds:DigestValue></ds:Reference></ds:SignedInfo><ds:SignatureValue>RkLC5skfz346fLRJeRPwhjj3W6XGlEuhpcBRdhWp1vYBneKewVdNo5AAsU3fzs5n+qlNE1slr+lzsA0bw/rlYSsgJkDheCYS7Ltg5rT2utEqkJS5IlnFnkNfq3wi/dTTLXbcWEDatijk21hKOkAMfGIWK5+jq8RmkFnsPxap2zHGTrWqs2nbRGP3iykSaoYciWjSvN88RQsmoJaT/yWO1xPoGwV800x6CDNVoQQ9+9UsBabTpJWxAx2bZE1TKoD8wxf12gTH0Or53N4hNWEuD/xKYjfTI+NV3wreQsLFW/zfVGHZ7VGkX+XBmvd34k++lfiwkKcqWGzT6omFMwbi8A==</ds:SignatureValue><KeyInfo xmlns=\"http://www.w3.org/2000/09/xmldsig#\"><ds:X509Data><ds:X509Certificate>MIIC4DCCAcigAwIBAgIQScrocMfxc4FJpePGt81dKDANBgkqhkiG9w0BAQsFADAsMSowKAYDVQQDEyFBREZTIFNpZ25pbmcgLSBzc28uaHEuc2NpcmV1bS5jb20wHhcNMjEwOTEzMTMzMTM3WhcNMjQwOTEyMTMzMTM3WjAsMSowKAYDVQQDEyFBREZTIFNpZ25pbmcgLSBzc28uaHEuc2NpcmV1bS5jb20wggEiMA0GCSqGSIb3DQEBAQUAA4IBDwAwggEKAoIBAQCaC+lpjegfdnsf7H4zxuI0jxBDK5Roq6A60y4QJ1f2ysHi/7Gn4DFavdJJ4wPvPbJzzkOykXVcgn+BmKfscRvy8h6pW21dVMwwxC5q/vnvui0rqZYCHD6HmMrR2rTEEO+w5W4AznFzRl/blo4QHFtwLWGscbGaeO7m9+VUjXZ5fO9lg7rngHw5jjVPtgu8d57p88zitFbbK/DWYcWoOgj98fubU9nLyRYJmHavoL5ZEWX7hesvXEaCjeaZuJUPEF8HYrlmc1+LWGNc2uUpXZC/qcOb3eD4JXPRQKpzGW8A3+wCnr3wtlJHM6at6h7ZBpRc2nvGkb/u892isn9dsL47AgMBAAEwDQYJKoZIhvcNAQELBQADggEBAEfA2AD48pzStBtgq/yLrLMHSfDAZiBQhrAn6/1veKDU80ruz1Rd/wpjS19rPLp7kjiaRSAp8aKQDeaQDRmMNczN958ry1P2vd12Teilp1zBo0uZ9ce7tAt+eV4kyh3mvyUzL5RGSpEBJ+Cf/gVo/zH+Yy/x07urHt0POmO5zaw/ZhW7dZoWHzCzJ/fjnJ+UlaLIlgvvT0TxyspadWqVeZLgL+nnC2vy+YYESWU3auv30ykkWgpc246Hy1+zsf0z5KIQJuwkVAjY5hpMWNclgTT4QBBFWY5zGgfPiNby76Eks4ryAA889X4teC61wiHaj1Ok3CZ4Y9iLYLg2OQzONgs=</ds:X509Certificate></ds:X509Data></KeyInfo></ds:Signature><Subject><SubjectConfirmation Method=\"urn:oasis:names:tc:SAML:2.0:cm:bearer\"><SubjectConfirmationData InResponseTo=\"identifier_1652354198578\" NotOnOrAfter=\"2022-05-12T11:22:00.648Z\" Recipient=\"https://memoio.staging.scireum.com/saml/login\" /></SubjectConfirmation></Subject><Conditions NotBefore=\"2022-05-12T11:17:00.648Z\" NotOnOrAfter=\"2022-05-12T12:17:00.648Z\"><AudienceRestriction><Audience>memoio-staging</Audience></AudienceRestriction></Conditions><AttributeStatement><Attribute Name=\"http://schemas.xmlsoap.org/ws/2005/05/identity/claims/emailaddress\"><AttributeValue>jvo@scireum.de</AttributeValue></Attribute><Attribute Name=\"http://schemas.microsoft.com/ws/2008/06/identity/claims/role\"><AttributeValue>administrator,user-administrator,jobs-manager,permission-manage-company,,permission-manage-channels,,permission-manage-broadcasts,,permission-manage-files,permission-manage-custom-folders,permission-wipe-users</AttributeValue></Attribute><Attribute Name=\"http://schemas.xmlsoap.org/ws/2005/05/identity/claims/name\"><AttributeValue>Jakob Vogel</AttributeValue></Attribute></AttributeStatement><AuthnStatement AuthnInstant=\"2022-05-12T11:17:00.570Z\"><AuthnContext><AuthnContextClassRef>urn:oasis:names:tc:SAML:2.0:ac:classes:PasswordProtectedTransport</AuthnContextClassRef></AuthnContext></AuthnStatement></Assertion></samlp:Response>",
                    true
            )
        }
        assertTrue(exception.message!!.contains("Invalid SAML Response: Invalid IssueInstant:"))
    }

    @Test
    fun `logind SAML response parses correctly when ignoring time`() {
        val response = parseSAMLResponse(
                """<?xml version="1.0"?>
<samlp:Response xmlns:samlp="urn:oasis:names:tc:SAML:2.0:protocol" ID="5c0fd610-715f-4187-8f5e-298ca16dd37d" Version="2.0" IssueInstant="" Destination="https://memoio.test.scireum.com/saml/login" Consent="urn:oasis:names:tc:SAML:2.0:consent:unspecified" InResponseTo="identifier_1652353644351"><Issuer xmlns="urn:oasis:names:tc:SAML:2.0:assertion">https://sso.hq.scireum.com/adfs/services/trust</Issuer><samlp:Status><samlp:StatusCode Value="urn:oasis:names:tc:SAML:2.0:status:Success"/></samlp:Status><Assertion xmlns="urn:oasis:names:tc:SAML:2.0:assertion" ID="_30e5c8bb-ee3a-4290-9960-f5fae240d0e0" IssueInstant="2022-05-12T11:07:26.759Z" Version="2.0"><Issuer>https://sso.hq.scireum.com/adfs/services/trust</Issuer><ds:Signature xmlns:ds="http://www.w3.org/2000/09/xmldsig#"><ds:SignedInfo><ds:CanonicalizationMethod Algorithm="http://www.w3.org/2001/10/xml-exc-c14n#"/><ds:SignatureMethod Algorithm="http://www.w3.org/2001/04/xmldsig-more#rsa-sha256"/><ds:Reference URI="#_30e5c8bb-ee3a-4290-9960-f5fae240d0e0"><ds:Transforms><ds:Transform Algorithm="http://www.w3.org/2000/09/xmldsig#enveloped-signature"/><ds:Transform Algorithm="http://www.w3.org/2001/10/xml-exc-c14n#"/></ds:Transforms><ds:DigestMethod Algorithm="http://www.w3.org/2001/04/xmlenc#sha256"/><ds:DigestValue>oSkdX5u1Xt5PoFYL4HB+3iN9Y5Flmo8PJcL72gLBPr0=</ds:DigestValue></ds:Reference></ds:SignedInfo><ds:SignatureValue>wK618x44fMUudGg4xnGw8gETbVxMYGv73YMpGuD5gS8mOcU1xS9t2gninPexZEzh
Srx68o+5c1vmUgen+Z/t94jfw35cBIOnbMkWXfs3aovSaKqUAgRcgT+c7pMYofX4
cv0aOoBNhH+S4HL/8eGYo3yS32vk/Ngm/KdMmteeFsL/6v5cCTrgViCvhqhLVccX
fhzy5Z2IN1SPc6fOXTN6eXTGwTz43T7kk0XOgFrx88bOdf3ffgSOaIsmYT4q9khs
a31GTO/akfFzeJwqXTVANokZ4Y2b5e2PQw+QJTuKMUYAGKWTpHOq+23a6TQwpeTo
XwEfZn3REAEJG1RkZc1tlA==</ds:SignatureValue><ds:KeyInfo xmlns="http://www.w3.org/2000/09/xmldsig#"><ds:X509Data>
<ds:X509Certificate>MIIDGzCCAgOgAwIBAgIUKWE56kp2noHiNxwSDkWFAfxQJAcwDQYJKoZIhvcNAQEL
BQAwHTEbMBkGA1UEAwwSbG9naW5kLnNjaXJldW0uY29tMB4XDTIyMDUwMzE4NTgw
OVoXDTMyMDQzMDE4NTgwOVowHTEbMBkGA1UEAwwSbG9naW5kLnNjaXJldW0uY29t
MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAzjx3Ov933PplnBfFMrr3
rxz11QKqqEOd2ewrCQunoxLLIJzw9RQWS8oAk8xO9FOm72RIlKU4SNa/PvBak+Ez
hOuFwwSzVGVLg8eZm05TAN+Zf0PCe1wI8a4l/lDhtVEr03Fe+hn0Pl1c+fd/TVyR
mo11io/ZWOISM/ILmfVlVAbrPdVcnNJ6DTC56j8OdniRK6mbKX70wjcTsnPMR4B1
MHSfxmPjycYXq9nYaurE+5Hdsda8+JzXxWua/0B0C7OI2IHoqZ3fwsulFrey/DEh
5NkCkyj/lbE1UU3YbetWR8C1/I6J0eFK6l5GYoofd+Zici2/dtDtIzDicVsq3EqG
iwIDAQABo1MwUTAdBgNVHQ4EFgQUfYTw8Bft7uoqY7kgRMZ6NkR7Jg8wHwYDVR0j
BBgwFoAUfYTw8Bft7uoqY7kgRMZ6NkR7Jg8wDwYDVR0TAQH/BAUwAwEB/zANBgkq
hkiG9w0BAQsFAAOCAQEASLXhVm3d/IGGhDcZAijDOM2g89W+IxVGWkfpnc797xKL
pc91m5k3E1nN0hxt/uxnfOqJr+iitdHM/+c6iBmSILygOLe+ATwNjEZ+AEJPd04k
GNa5lupf6Fr4poJ3lEclDET9peKYAQdh71iY7e0TLg+1W/lOCvNCnmgmY9vmSTzE
Ps0C4RbuuJagURKHTqc757OawimGZvN0dA2JJ7c6m0gjFxeQ2pX0bVOAGmVeD6FR
UtS2kvA28X4ToQg3REfK8K+MroixIpwVfdyHRCP4CsLrz4w+EJw4VlWAzJ45HFHg
15uMlXcnBt42Bmx7ifeCuOhqgdTHHWpMtSq23dIS/A==</ds:X509Certificate>
</ds:X509Data></ds:KeyInfo></ds:Signature><Subject><NameID>jge@scireum.de</NameID><SubjectConfirmation Method="urn:oasis:names:tc:SAML:2.0:cm:bearer"><SubjectConfirmationData InResponseTo="identifier_1652353644351" NotOnOrAfter="2022-05-12T13:07:26.759Z" Recipient="https://sellsite.test.scireum.com/saml/login"/></SubjectConfirmation></Subject><Conditions NotBefore="2022-05-12T09:07:26.759Z" NotOnOrAfter="2022-05-12T13:07:26.759Z"><AudienceRestriction><Audience>sellsite-test</Audience></AudienceRestriction></Conditions><AuthnStatement AuthnInstant="2022-05-12T11:07:26.759Z" SessionIndex="_30e5c8bb-ee3a-4290-9960-f5fae240d0e0"><AuthnContext><AuthnContextClassRef>urn:oasis:names:tc:SAML:2.0:ac:classes:PasswordProtectedTransport</AuthnContextClassRef></AuthnContext></AuthnStatement><AttributeStatement><Attribute Name="http://schemas.xmlsoap.org/ws/2005/05/identity/claims/emailaddress"><AttributeValue>jvo@scireum.de</AttributeValue></Attribute><Attribute Name="http://schemas.xmlsoap.org/ws/2005/05/identity/claims/surname"><AttributeValue>Vogel</AttributeValue></Attribute><Attribute Name="http://schemas.xmlsoap.org/ws/2005/05/identity/claims/givenname"><AttributeValue>Jakob</AttributeValue></Attribute><Attribute Name="http://schemas.microsoft.com/ws/2008/06/identity/claims/role"><AttributeValue>administrator,user-administrator,jobs-manager,permission-manage-company,,permission-manage-channels,,permission-manage-broadcasts,,permission-manage-files,permission-manage-custom-folders,permission-wipe-users</AttributeValue><AttributeValue>permission-manage-custom-folders</AttributeValue></Attribute><Attribute Name="http://schemas.xmlsoap.org/ws/2005/05/identity/claims/name"><AttributeValue>Jakob Vogel via G</AttributeValue></Attribute></AttributeStatement></Assertion></samlp:Response>
""",
                false
        )
        assertEquals(
                "71038506714cb316a8cb6500b26551b1c29375ce",
                response.fingerprint
        )
        assertEquals(
                "https://sso.hq.scireum.com/adfs/services/trust",
                response.issuer
        )
    }

}
