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
 * Provides a macro which inlines an SVG file.
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

        Resource resource = resources.resolve(path).orElse(null);
        if (resource == null) {
            return "";
        }

        Element root = parseDocument(resource).getDocumentElement();
        if (!Strings.areEqual("svg", root.getTagName())) {
            throw Exceptions.createHandled().withDirectMessage("The referenced resource is not an SVG file.").handle();
        }

        return stringifyElement(root);
    }

    private Document parseDocument(Resource resource) {
        try (InputStream stream = resource.openStream()) {
            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            return factory.newDocumentBuilder().parse(stream);
        } catch (Exception exception) {
            throw Exceptions.handle(exception);
        }
    }

    private String stringifyElement(Element element) {
        try (StringWriter writer = new StringWriter()) {
            TransformerFactory factory = TransformerFactory.newInstance();
            Transformer transformer = factory.newTransformer();
            transformer.setOutputProperty("omit-xml-declaration", "yes");
            transformer.transform(new DOMSource(element), new StreamResult(writer));
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
        return "Returns the root <svg> tag from an SVG file as string.";
    }
}
