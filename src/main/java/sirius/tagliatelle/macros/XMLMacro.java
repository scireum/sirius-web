/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.macros;

import org.w3c.dom.Document;
import org.xml.sax.InputSource;
import sirius.kernel.di.std.Register;
import sirius.kernel.xml.StructuredNode;
import sirius.tagliatelle.Tagliatelle;
import sirius.tagliatelle.expression.Expression;
import sirius.tagliatelle.rendering.LocalRenderContext;

import javax.annotation.Nonnull;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import java.io.StringReader;
import java.util.List;

/**
 * Parses the given XML string into a {@link StructuredNode}.
 */
@Register
public class XMLMacro implements Macro {

    @Override
    public Class<?> getType() {
        return StructuredNode.class;
    }

    @Override
    public void verifyArguments(List<Expression> args) {
        if (args.size() != 1 || !Tagliatelle.isAssignableTo(args.get(0).getType(), String.class)) {
            throw new IllegalArgumentException("Expected a single String as argument.");
        }
    }

    @Override
    public Object eval(LocalRenderContext ctx, Expression[] args) {
        try {
            String xmlString = (String) args[0].eval(ctx);
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
    public boolean isConstant(Expression[] args) {
        return true;
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
}
