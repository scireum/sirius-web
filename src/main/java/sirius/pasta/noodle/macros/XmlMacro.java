/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.macros;

import org.w3c.dom.Document;
import sirius.kernel.tokenizer.Position;
import sirius.kernel.di.std.Register;
import sirius.kernel.xml.StructuredNode;
import sirius.pasta.noodle.Environment;
import sirius.pasta.noodle.compiler.CompilationContext;
import sirius.pasta.noodle.compiler.ir.Node;
import sirius.pasta.noodle.sandbox.NoodleSandbox;

import javax.annotation.Nonnull;
import java.util.List;

/**
 * Parses the given XML string into a {@link StructuredNode}.
 */
@Register
@NoodleSandbox(NoodleSandbox.Accessibility.GRANTED)
public class XmlMacro extends XmlProcessingMacro {

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
            Document doc = parseDocument(xmlString);
            return StructuredNode.of(doc.getDocumentElement());
        } catch (Exception e) {
            throw new IllegalArgumentException("Invalid xml: " + e.getMessage(), e);
        }
    }

    @Override
    public String getDescription() {
        return "Parses the given XML string into a StructuredNode";
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
