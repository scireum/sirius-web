/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.macros;

import org.w3c.dom.Document;
import org.xml.sax.InputSource;
import sirius.kernel.tokenizer.Position;
import sirius.kernel.di.std.Register;
import sirius.kernel.xml.StructuredNode;
import sirius.pasta.noodle.Environment;
import sirius.pasta.noodle.compiler.CompilationContext;
import sirius.pasta.noodle.compiler.ir.Node;
import sirius.pasta.noodle.sandbox.PublicApi;

import javax.annotation.Nonnull;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import java.io.StringReader;
import java.util.List;

/**
 * Parses the given XML string into a {@link StructuredNode}.
 */
@Register
@PublicApi
public class XmlMacro extends BasicMacro {

    @Override
    public Class<?> getType() {
        return StructuredNode.class;
    }

    @Override
    public void verifyArguments(CompilationContext context, Position position, List<Class<?>> args) {
        if (args.size() != 1 || !CompilationContext.isAssignableTo(args.get(0), String.class)) {
            throw new IllegalArgumentException("Expected a single String as argument.");
        }
    }

    @Override
    public Object invoke(Environment environment, Object[] args) {
        try {
            String xmlString = (String) args[0];
            Document doc = parse(xmlString);
            return StructuredNode.of(doc.getDocumentElement());
        } catch (Exception e) {
            throw new IllegalArgumentException("Invalid xml: " + e.getMessage(), e);
        }
    }

    private Document parse(String xmlString) throws Exception {
        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        DocumentBuilder builder = factory.newDocumentBuilder();
        InputSource is = new InputSource(new StringReader(xmlString));
        return builder.parse(is);
    }

    @Override
    public String getDescription() {
        return "Parses the given XML string into a SturcturedNode";
    }

    @Nonnull
    @Override
    public String getName() {
        return "xml";
    }

    @Override
    public boolean isConstant(CompilationContext context, List<Node> args) {
        return true;
    }

}
