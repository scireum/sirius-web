/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.security;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;
import sirius.kernel.commons.Explain;
import sirius.kernel.commons.Hasher;
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
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.security.Key;
import java.security.cert.X509Certificate;
import java.time.Duration;
import java.time.Instant;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.util.Base64;
import java.util.List;

/**
 * Provides a helper to generate SAML 2 requests and to process responses.
 * <p>
 * The <b>Security Assertion Markup Language</b> is an XML beast to permit identity federation across cloud services.
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
     * in daylight saving time settings.
     */
    public static final int MAX_TIMESTAMP_DELTA_IN_HOURS = 3;

    private static final String SAML_NAMESPACE = "urn:oasis:names:tc:SAML:2.0:assertion";

    private static final String SAMLP_NAMESPACE = "urn:oasis:names:tc:SAML:2.0:protocol";

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
        return Base64.getEncoder().encodeToString(createAuthenticationRequestXML(issuer, issuerIndex));
    }

    private byte[] createAuthenticationRequestXML(String issuer, String issuerIndex) {
        ByteArrayOutputStream buffer = new ByteArrayOutputStream();
        XMLStructuredOutput output = new XMLStructuredOutput(buffer);
        output.beginOutput("samlp:AuthnRequest",
                           Attribute.set("xmlns:samlp", SAMLP_NAMESPACE),
                           Attribute.set("xmlns:saml", SAML_NAMESPACE),
                           Attribute.set("ID", "identifier_" + System.currentTimeMillis()),
                           Attribute.set("Version", "2.0"),
                           Attribute.set("IssueInstant",
                                         DateTimeFormatter.ISO_INSTANT.format(Instant.now()
                                                                                     .truncatedTo(ChronoUnit.SECONDS))),
                           Attribute.set("AssertionConsumerServiceIndex", issuerIndex));
        output.property("saml:Issuer", issuer);
        output.beginObject("samlp:NameIDPolicy",
                           Attribute.set("AllowCreate", false),
                           Attribute.set("Format", "urn:oasis:names:tc:SAML:1.1:nameid-format:unspecified"));
        output.endObject();
        output.endOutput();

        if (LOG.isFINE()) {
            LOG.FINE("Generating SAML request: %s", buffer.toString(StandardCharsets.UTF_8));
        }

        return buffer.toByteArray();
    }

    /**
     * Parses a SAML 2 response from the given request.
     * <p>
     * Note that the fingerprint <b>must</b> be verified in some way or another, as this method only checks if
     * the signature is valid, not <b>who</b> created it.
     *
     * @param webContext the http request to read the response from
     * @return the parsed response which has been verified
     */
    public SAMLResponse parseSAMLResponse(WebContext webContext) {
        if (!webContext.isUnsafePOST()) {
            throw Exceptions.createHandled().withSystemErrorMessage("Invalid SAML Response: POST expected!").handle();
        }

        byte[] response = Base64.getDecoder().decode(webContext.get("SAMLResponse").asString());

        if (LOG.isFINE()) {
            LOG.FINE("Received SAML response: %s", new String(response, StandardCharsets.UTF_8));
        }

        try (InputStream input = new ByteArrayInputStream(response)) {
            return parseSAMLResponse(input, true);
        } catch (HandledException exception) {
            throw exception;
        } catch (Exception exception) {
            throw Exceptions.handle()
                            .to(LOG)
                            .error(exception)
                            .withSystemErrorMessage("An error occurred while parsing a SAML Response: %s (%s)")
                            .handle();
        }
    }

    /**
     * Parses a SAML 2 response from the given input string, optionally checking timestamps.
     * <p>
     * Note that the fingerprint <b>must</b> be verified in some way or another, as this method only checks if
     * the signature is valid, not <b>who</b> created it.
     *
     * @param inputStream a stream containing the SAML XML response to parse
     * @param checkTime   a flag indicating whether to check for expired timestamps
     * @return the parsed response which has been verified
     */
    public SAMLResponse parseSAMLResponse(InputStream inputStream, boolean checkTime) {
        try {
            Document document = getResponseDocument(inputStream);

            Element assertion = selectSingleElement(document, SAML_NAMESPACE, "Assertion");
            if (checkTime) {
                verifyTimestamp(assertion);
            }
            String fingerprint = validateXMLSignature(document, assertion);

            return parseAssertion(assertion, fingerprint);
        } catch (HandledException exception) {
            throw exception;
        } catch (Exception exception) {
            throw Exceptions.handle()
                            .to(LOG)
                            .error(exception)
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
     * @param document  the XML document
     * @param namespace the optional namespace URI
     * @param nodeName  the name of the node
     * @return the element with the given name
     * @throws HandledException if there are either no or multiple nodes of the given name
     */
    private Element selectSingleElement(Document document, @Nullable String namespace, String nodeName) {
        NodeList nodes = Strings.isFilled(namespace) ?
                         document.getElementsByTagNameNS(namespace, nodeName) :
                         document.getElementsByTagName(nodeName);
        if (nodes.getLength() != 1) {
            LOG.FINE("SAML Response has %s elements of type: %s", nodes.getLength(), nodeName);
            throw Exceptions.createHandled()
                            .withSystemErrorMessage("Invalid SAML Response: Expected exactly one %s!", nodeName)
                            .handle();
        }

        return (Element) nodes.item(0);
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
        if (Duration.between(parsedIssueInstant, Instant.now()).toHours() >= MAX_TIMESTAMP_DELTA_IN_HOURS) {
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

    private Document getResponseDocument(InputStream inputStream)
            throws SAXException, IOException, ParserConfigurationException {
        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        factory.setNamespaceAware(true);
        return factory.newDocumentBuilder().parse(inputStream);
    }

    /**
     * Validates the <tt>Signature</tt> in the document.
     * <p>
     * This also ensures, that there is only one signature and that the signature actually signs the given
     * <tt>Assertion</tt> and not anything else.
     *
     * @param document  the XML document
     * @param assertion the assertion which must be signed
     * @return the fingerprint of the X509 certificate which was used to generate the signature
     * @throws Exception in case an XML or signing error occurs
     */
    private String validateXMLSignature(Document document, Element assertion) throws Exception {
        assertion.setIdAttribute("ID", true);
        String idToVerify = assertion.getAttribute("ID");

        Element signatureElement = selectSingleElement(document, XMLSignature.XMLNS, "Signature");

        XMLSignatureFactory factory = XMLSignatureFactory.getInstance("DOM");
        DOMValidateContext valContext = new DOMValidateContext(new KeyValueKeySelector(), signatureElement);
        XMLSignature signature = factory.unmarshalXMLSignature(valContext);

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

        X509Certificate certificate = ((X509CertificateResult) signature.getKeySelectorResult()).getCertificate();
        return Hasher.sha1().hashBytes(certificate.getEncoded()).toHexString().toLowerCase();
    }

    /**
     * Obtains the reference of the given signature
     *
     * @param signature the signature to parse
     * @return the effective reference URI
     */
    @SuppressWarnings({"squid:S1905", "RedundantCast"})
    @Nonnull
    @Explain("The cast helps the type-interference of the compiler - otherwise it sometimes reports an error")
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

        @Override
        public KeySelectorResult select(KeyInfo keyInfo,
                                        KeySelector.Purpose purpose,
                                        AlgorithmMethod method,
                                        XMLCryptoContext cryptoContext) throws KeySelectorException {
            if (keyInfo == null) {
                throw Exceptions.createHandled()
                                .withSystemErrorMessage("Invalid SAML Response: Signature doesn't contain a KeyInfo!")
                                .handle();
            }

            for (XMLStructure xmlStructure : keyInfo.getContent()) {
                if (xmlStructure instanceof X509Data x509Data && !x509Data.getContent().isEmpty()) {
                    X509Certificate x509Certificate = (X509Certificate) x509Data.getContent().getFirst();
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

        private final X509Certificate certificate;

        X509CertificateResult(X509Certificate cert) {
            this.certificate = cert;
        }

        @Override
        public Key getKey() {
            return certificate.getPublicKey();
        }

        public X509Certificate getCertificate() {
            return certificate;
        }
    }
}
