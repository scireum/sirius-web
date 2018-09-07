/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.security;

import com.google.common.base.Charsets;
import com.google.common.hash.Hashing;
import com.google.common.io.BaseEncoding;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;
import sirius.kernel.commons.MultiMap;
import sirius.kernel.commons.Strings;
import sirius.kernel.di.std.Register;
import sirius.kernel.health.Exceptions;
import sirius.kernel.health.HandledException;
import sirius.kernel.health.Log;
import sirius.kernel.xml.Attribute;
import sirius.kernel.xml.StructuredNode;
import sirius.kernel.xml.XMLStructuredOutput;
import sirius.web.http.WebContext;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import javax.xml.crypto.AlgorithmMethod;
import javax.xml.crypto.KeySelector;
import javax.xml.crypto.KeySelectorException;
import javax.xml.crypto.KeySelectorResult;
import javax.xml.crypto.XMLCryptoContext;
import javax.xml.crypto.XMLStructure;
import javax.xml.crypto.dsig.Reference;
import javax.xml.crypto.dsig.XMLSignature;
import javax.xml.crypto.dsig.XMLSignatureFactory;
import javax.xml.crypto.dsig.dom.DOMValidateContext;
import javax.xml.crypto.dsig.keyinfo.KeyInfo;
import javax.xml.crypto.dsig.keyinfo.X509Data;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.security.Key;
import java.security.cert.X509Certificate;
import java.time.Duration;
import java.time.Instant;
import java.time.format.DateTimeFormatter;
import java.util.List;

/**
 * Provides a helper to generate SAML 2 requests and to process responses.
 * <p>
 * The <b>Security Assertion Markup Language</b> is an XML beast to permit identity federation accross cloud services.
 * This implementation provides all the tools required to become an identity consumer, which can call an identity
 * provider to authenticate a user.
 */
@Register(classes = SAMLHelper.class)
public class SAMLHelper {

    /**
     * Used to log all events related to SAML.
     * <p>
     * Some FINE loggings are provided which might support troubleshooting.
     */
    public static final Log LOG = Log.get("saml");

    /**
     * A response as a timestamp called <tt>IssueInstant</tt>. We enforce that a received response isn't older than
     * <tt>MAX_TIMESTAMP_DELTA_IN_HOURS</tt> hours. The value should be chosen to accept clock drift and differences
     * in dailight saving time settings.
     */
    public static final int MAX_TIMESTAMP_DELTA_IN_HOURS = 3;

    /**
     * Generates a base64 encoded XML request which can be POSTed to a SAML 2 identity provider.
     *
     * @param issuer      the name of the issuer. This tells the identity provider "who" is asking to perform an authentication.
     * @param issuerIndex the index of the issuer. As the identity provider might manage several endpoints for a
     *                    single issuer configuration, different indices can be passed in. The default value would
     *                    be "0"
     * @return a base64 encoded SAML2 request which can be posted to an identity provider
     */
    public String generateAuthenticationRequest(String issuer, String issuerIndex) {
        return BaseEncoding.base64().encode(createAuthenticationRequestXML(issuer, issuerIndex));
    }

    private byte[] createAuthenticationRequestXML(String issuer, String issuerIndex) {
        ByteArrayOutputStream buffer = new ByteArrayOutputStream();
        XMLStructuredOutput out = new XMLStructuredOutput(buffer);
        out.beginOutput("samlp:AuthnRequest",
                        Attribute.set("xmlns:samlp", "urn:oasis:names:tc:SAML:2.0:protocol"),
                        Attribute.set("xmlns:saml", "urn:oasis:names:tc:SAML:2.0:assertion"),
                        Attribute.set("ID", "identifier_" + System.currentTimeMillis()),
                        Attribute.set("Version", "2.0"),
                        Attribute.set("IssueInstant", DateTimeFormatter.ISO_INSTANT.format(Instant.now())),
                        Attribute.set("AssertionConsumerServiceIndex", issuerIndex));
        out.property("saml:Issuer", issuer);
        out.beginObject("samlp:NameIDPolicy",
                        Attribute.set("AllowCreate", false),
                        Attribute.set("Format", "urn:oasis:names:tc:SAML:1.1:nameid-format:unspecified"));
        out.endObject();
        out.endOutput();

        if (LOG.isFINE()) {
            LOG.FINE("Generating SAML request: %s", new String(buffer.toByteArray(), Charsets.UTF_8));
        }

        return buffer.toByteArray();
    }

    /**
     * Parses a SAML 2 response from the given request.
     * <p>
     * Note that the fingerprint <b>must</b> be verified in some way or another, as this method only checks if
     * the signature is valid, not <b>who</b> created it.
     *
     * @param ctx the http request to read the response from
     * @return the parsed response which has been verified
     */
    public SAMLResponse parseSAMLResponse(WebContext ctx) {
        if (!ctx.isUnsafePOST()) {
            throw Exceptions.createHandled().withSystemErrorMessage("Invalid SAML Response: POST expected!").handle();
        }

        try {
            Document doc = getResponseDocument(ctx);

            Element assertion = selectSingleElement(doc, null, "Assertion");
            verifyTimestamp(assertion);
            String fingerprint = validateXMLSignature(doc, assertion);

            return parseAssertion(assertion, fingerprint);
        } catch (HandledException e) {
            throw e;
        } catch (Exception e) {
            throw Exceptions.handle()
                            .to(LOG)
                            .error(e)
                            .withSystemErrorMessage("An error occurred while parsing a SAML Response: %s (%s)")
                            .handle();
        }
    }

    /**
     * Selects the element with the given node name.
     * <p>
     * This method ensures, that there is only one element, as we want to ensure that there is only on <tt>Assertion</tt> and
     * one <tt>Signature</tt> for it.
     *
     * @param doc       the XML document
     * @param namespace the optional namesapce URI
     * @param nodeName  the name of the node
     * @return the element with the given name
     * @throws HandledException if there are zero or more thant one nodes found
     */
    private Element selectSingleElement(Document doc, @Nullable String namespace, String nodeName) {
        NodeList nl = Strings.isFilled(namespace) ?
                      doc.getElementsByTagNameNS(namespace, nodeName) :
                      doc.getElementsByTagName(nodeName);
        if (nl.getLength() != 1) {
            LOG.FINE("SAML Response has %s elements of type: %s", nl.getLength(), nodeName);
            throw Exceptions.createHandled()
                            .withSystemErrorMessage("Invalid SAML Response: Expected exactly one %s!", nodeName)
                            .handle();
        }

        return (Element) nl.item(0);
    }

    /**
     * Verifies the <tt>IssueInstant</tt> within the <tt>Assertion</tt>.
     *
     * @param assertion the assertion to verify
     * @see #MAX_TIMESTAMP_DELTA_IN_HOURS
     */
    private void verifyTimestamp(Element assertion) {
        String issueInstant = assertion.getAttribute("IssueInstant");
        Instant parsedIssueInstant = Instant.from(DateTimeFormatter.ISO_INSTANT.parse(issueInstant));
        if (Duration.between(Instant.now(), parsedIssueInstant).toHours() > MAX_TIMESTAMP_DELTA_IN_HOURS) {
            throw Exceptions.createHandled()
                            .withSystemErrorMessage("Invalid SAML Response: Invalid IssueInstant: %s", issueInstant)
                            .handle();
        }
    }

    /**
     * Extracts all relevant data from the <tt>Assertion</tt>.
     *
     * @param assertion   the assertion to parse
     * @param fingerprint the fingerprint of the X509 certificate which was used to sign the assertion
     * @return the response which represents the payload of the <tt>Assertion</tt>
     */
    private SAMLResponse parseAssertion(Element assertion, String fingerprint) {
        StructuredNode node = StructuredNode.of(assertion);
        MultiMap<String, String> attributes = MultiMap.create();

        for (StructuredNode attribute : node.queryNodeList(
                "*[local-name()='AttributeStatement']/*[local-name()='Attribute']")) {
            for (StructuredNode attributeValue : attribute.queryNodeList("*[local-name()='AttributeValue']")) {
                attributes.put(attribute.queryString("@Name"), attributeValue.queryString("."));
            }
        }

        return new SAMLResponse(node.queryString("*[local-name()='Issuer']"),
                                fingerprint,
                                node.queryString("*[local-name()='Subject']/*[local-name()='NameID']"),
                                attributes);
    }

    private Document getResponseDocument(WebContext ctx)
            throws SAXException, IOException, ParserConfigurationException {
        byte[] response = BaseEncoding.base64().decode(ctx.get("SAMLResponse").asString());

        if (LOG.isFINE()) {
            LOG.FINE("Received SAML response: %s", new String(response, Charsets.UTF_8));
        }

        DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        dbf.setNamespaceAware(true);
        return dbf.newDocumentBuilder().parse(new ByteArrayInputStream(response));
    }

    /**
     * Validates the <tt>Signature</tt> in the document.
     * <p>
     * This also ensures, that there is only one signature and that the signature actually signs the given
     * <tt>Assertion</tt> and not anything else.
     *
     * @param doc       the XML document
     * @param assertion the assertion which must be signed
     * @return the fingerprint of the X509 certificate which was used to generate the signature
     * @throws Exception in case an XML or signung error occurs
     */
    private String validateXMLSignature(Document doc, Element assertion) throws Exception {
        assertion.setIdAttribute("ID", true);
        String idToVerify = assertion.getAttribute("ID");

        Element signatureElement = selectSingleElement(doc, XMLSignature.XMLNS, "Signature");

        XMLSignatureFactory fac = XMLSignatureFactory.getInstance("DOM");
        DOMValidateContext valContext = new DOMValidateContext(new KeyValueKeySelector(), signatureElement);
        XMLSignature signature = fac.unmarshalXMLSignature(valContext);

        if (!Strings.areEqual(getReferenceBeingSigned(signature), "#" + idToVerify)) {
            LOG.FINE("SAML Response doesn't sign the assertion. Reference: %s, Assertion-ID: %s",
                     getReferenceBeingSigned(signature),
                     idToVerify);
            throw Exceptions.createHandled()
                            .withSystemErrorMessage(
                                    "Invalid SAML Response: The given Signature doesn't sign the given Assertion.")
                            .handle();
        }

        if (!signature.validate(valContext)) {
            LOG.FINE("SAML Response contains an invalid signature!");
            throw Exceptions.createHandled()
                            .withSystemErrorMessage("Invalid SAML Response: The given Signature isn't valid.")
                            .handle();
        }

        X509Certificate certificate = ((X509CertificateResult) signature.getKeySelectorResult()).getCert();
        return Hashing.sha1().hashBytes(certificate.getEncoded()).toString().toLowerCase();
    }

    /**
     * Obtains the reference of the given signature
     *
     * @param signature the signature to parse
     * @return the effective reference URI
     */
    @SuppressWarnings("unchecked")
    @Nonnull
    private String getReferenceBeingSigned(XMLSignature signature) {
        return ((List<Reference>) signature.getSignedInfo().getReferences()).stream()
                                                                            .findFirst()
                                                                            .map(Reference::getURI)
                                                                            .orElse("");
    }

    /**
     * Used to extract the inlined X509 certificate from within the signature.
     */
    private static class KeyValueKeySelector extends KeySelector {

        @SuppressWarnings("unchecked")
        @Override
        public KeySelectorResult select(KeyInfo keyInfo,
                                        KeySelector.Purpose purpose,
                                        AlgorithmMethod method,
                                        XMLCryptoContext context) throws KeySelectorException {
            if (keyInfo == null) {
                throw Exceptions.createHandled()
                                .withSystemErrorMessage("Invalid SAML Response: Signature doesn't contain a KeyInfo!")
                                .handle();
            }

            for (XMLStructure xmlStructure : (List<XMLStructure>) keyInfo.getContent()) {
                if (xmlStructure instanceof X509Data) {
                    X509Certificate x509Certificate = (X509Certificate) ((X509Data) xmlStructure).getContent().get(0);
                    return new X509CertificateResult(x509Certificate);
                }
            }

            throw Exceptions.createHandled()
                            .withSystemErrorMessage(
                                    "Invalid SAML Response: Signature doesn't contain a valid signing key (X509 certificate)!")
                            .handle();
        }
    }

    /**
     * Represents teh inlined X509 certificate from within the signature.
     */
    private static class X509CertificateResult implements KeySelectorResult {

        private X509Certificate cert;

        X509CertificateResult(X509Certificate cert) {
            this.cert = cert;
        }

        @Override
        public Key getKey() {
            return cert.getPublicKey();
        }

        public X509Certificate getCert() {
            return cert;
        }
    }
}

