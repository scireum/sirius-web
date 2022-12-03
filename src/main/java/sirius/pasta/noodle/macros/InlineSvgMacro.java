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
import sirius.kernel.commons.Strings;
import sirius.kernel.di.std.Part;
import sirius.kernel.di.std.Register;
import sirius.kernel.health.Exceptions;
import sirius.kernel.tokenizer.Position;
import sirius.pasta.noodle.Environment;
import sirius.pasta.noodle.compiler.CompilationContext;
import sirius.pasta.noodle.compiler.ir.Node;
import sirius.web.resources.Resource;
import sirius.web.resources.Resources;

import javax.annotation.Nonnull;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import java.io.InputStream;
import java.io.StringWriter;
import java.util.List;

/**
 * Provides a macro for inlining an SVG file into a DOM tree by dropping the XML declaration from the beginning and
 * returning the rest.
 */
@Register
public class InlineSvgMacro extends BasicMacro {

    @Part
    private Resources resources;

    @Override
    protected Class<?> getType() {
        return String.class;
    }

    @Override
    protected void verifyArguments(CompilationContext compilationContext, Position position, List<Class<?>> args) {
        if (args.size() != 1 || !CompilationContext.isAssignableTo(args.get(0), String.class)) {
            throw new IllegalArgumentException("Expected a single String as argument.");
        }
    }

    @Override
    public void verify(CompilationContext context, Position position, List<Node> args) {
        super.verify(context, position, args);

        if (args.get(0).isConstant()) {
            String resourceName = String.valueOf(args.get(0).getConstantValue());
            if (resources.resolve(resourceName).isEmpty()) {
                context.warning(position, "Unknown resource: %s", resourceName);
            }
        }
    }

    @Override
    public boolean isConstant(CompilationContext context, List<Node> args) {
        return args.get(0).isConstant();
    }

    @Override
    public Object invoke(Environment environment, Object[] args) {
        String path = (String) args[0];
        return processResource(path);
    }

    @Nonnull
    private String processResource(String path) {
        if (!path.startsWith("/assets")) {
            throw new IllegalArgumentException("Only assets can be inlined for security reasons.");
        }

        return stringifyElement(parseSvgDocument(resources.resolve(path)
                                                          .orElseThrow(() -> new IllegalArgumentException(
                                                                  "Unknown resource: " + path))).getDocumentElement(),
                                false);
    }

    /**
     * Parses an SVG DOM tree from the given resource.
     *
     * @param resource the resource to parse
     * @return the parsed DOM tree
     * @throws HandledException when parsing errors occur
     */
    public static Document parseSvgDocument(Resource resource) {
        try (InputStream stream = resource.openStream()) {
            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();

            Document document = factory.newDocumentBuilder().parse(stream);
            if (!Strings.areEqual("svg", document.getDocumentElement().getTagName())) {
                throw Exceptions.createHandled()
                                .withDirectMessage("The referenced resource is not an SVG file.")
                                .handle();
            }

            return document;
        } catch (Exception exception) {
            throw Exceptions.handle(exception);
        }
    }

    /**
     * Converts a DOM tree, given the {@code root}, to an XML string, optionally with or without a leading XML
     * declaration like {@code <?xml version="1.0" encoding="UTF-8" ?>}.
     *
     * @param root               the tree's root element
     * @param withXmlDeclaration <b>true</b> to synthesize a leading XML declaration, <b>false</b> else
     * @return a {@link String} representation of the DOM tree
     */
    public static String stringifyElement(Element root, boolean withXmlDeclaration) {
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

    @Nonnull
    @Override
    public String getName() {
        return "inlineSVG";
    }

    @Nonnull
    @Override
    public String getDescription() {
        return "Returns the root <svg> tag of an SVG file as string, to be included in other DOM trees.";
    }
}
