/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.macros;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;
import sirius.kernel.commons.Strings;
import sirius.kernel.health.Exceptions;
import sirius.kernel.health.HandledException;
import sirius.web.resources.Resource;

import javax.annotation.Nullable;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import java.io.InputStream;
import java.io.StringReader;
import java.io.StringWriter;

/**
 * Provides auxiliary methods for processing XML DOM trees.
 */
public abstract class XmlProcessingMacro extends BasicMacro {

    /**
     * Parses a DOM tree from the given string.
     *
     * @param string the string to parse
     * @return the parsed DOM tree
     * @throws HandledException when parsing errors occur
     */
    protected Document parseDocument(String string) {
        return parseDocument(string, null);
    }

    /**
     * Parses a DOM tree from the given string, optionally checking the name of the document element.
     *
     * @param string              the string to parse
     * @param expectedRootElement the expected name of the document element, or <b>null</b>
     * @return the parsed DOM tree
     * @throws HandledException when parsing errors occur
     */
    protected Document parseDocument(String string, String expectedRootElement) {
        return parseDocument(new InputSource(new StringReader(string)), expectedRootElement);
    }

    /**
     * Parses a DOM tree from the given resource.
     *
     * @param resource the resource to parse
     * @return the parsed DOM tree
     * @throws HandledException when parsing errors occur
     */
    protected Document parseDocument(Resource resource) {
        return parseDocument(resource, null);
    }

    /**
     * Parses a DOM tree from the given resource, optionally checking the name of the document element.
     *
     * @param resource            the resource to parse
     * @param expectedRootElement the expected name of the document element, or <b>null</b>
     * @return the parsed DOM tree
     * @throws HandledException when parsing errors occur
     */
    protected Document parseDocument(Resource resource, String expectedRootElement) {
        try (InputStream stream = resource.openStream()) {
            return parseDocument(new InputSource(stream), expectedRootElement);
        } catch (Exception e) {
            throw Exceptions.handle(e);
        }
    }

    /**
     * Parses a DOM tree from the given source.
     *
     * @param source the source to parse
     * @return the parsed DOM tree
     * @throws HandledException when parsing errors occur
     */
    protected Document parseDocument(InputSource source) {
        return parseDocument(source, null);
    }

    /**
     * Parses a DOM tree from the given source, optionally checking the name of the document element.
     *
     * @param source              the source to parse
     * @param expectedRootElement the expected name of the document element, or <b>null</b>
     * @return the parsed DOM tree
     * @throws HandledException when parsing errors occur
     */
    protected Document parseDocument(InputSource source, @Nullable String expectedRootElement) {
        try {
            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            Document document = factory.newDocumentBuilder().parse(source);

            if (Strings.isFilled(expectedRootElement) && !Strings.areEqual(expectedRootElement,
                                                                           document.getDocumentElement()
                                                                                   .getTagName())) {
                throw Exceptions.createHandled()
                                .withSystemErrorMessage("The referenced XML resource does not have a <%s> root element.",
                                                        expectedRootElement)
                                .handle();
            }

            return document;
        } catch (Exception exception) {
            throw Exceptions.handle(exception);
        }
    }

    /**
     * Recursively cleans indentation and newlines from a DOM tree, given the {@code root}.
     *
     * @param root the tree's root element
     * @return a convenience reference to the given {@code root} element
     */
    protected Element cleanIndentationAndNewlines(Element root) {
        NodeList children = root.getChildNodes();
        for (int i = 0; i < children.getLength(); ++i) {
            org.w3c.dom.Node child = children.item(i);
            if (child.getNodeType() == org.w3c.dom.Node.TEXT_NODE) {
                child.setTextContent(child.getTextContent().trim());
            } else if (child.getNodeType() == org.w3c.dom.Node.ELEMENT_NODE) {
                cleanIndentationAndNewlines((Element) child);
            }
        }
        return root;
    }

    /**
     * Converts a DOM tree, given the {@code root}, to an XML string, optionally with or without a leading XML
     * declaration like {@code <?xml version="1.0" encoding="UTF-8" ?>}.
     *
     * @param root               the tree's root element
     * @param withXmlDeclaration <b>true</b> to synthesize a leading XML declaration, <b>false</b> else
     * @return a {@link String} representation of the DOM tree
     */
    protected String stringifyElement(Element root, boolean withXmlDeclaration) {
        try (StringWriter writer = new StringWriter()) {
            TransformerFactory factory = TransformerFactory.newInstance();

            Transformer transformer = factory.newTransformer();
            if (!withXmlDeclaration) {
                transformer.setOutputProperty("omit-xml-declaration", "yes");
            }

            transformer.transform(new DOMSource(root), new StreamResult(writer));
            return writer.toString();
        } catch (Exception exception) {
            throw Exceptions.handle(exception);
        }
    }
}
