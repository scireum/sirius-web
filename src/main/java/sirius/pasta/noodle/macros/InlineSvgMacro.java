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
import sirius.pasta.noodle.Environment;
import sirius.pasta.noodle.compiler.CompilationContext;
import sirius.pasta.noodle.compiler.ir.Node;

import javax.annotation.Nonnull;
import java.util.List;

/**
 * Provides an abstract macro for inlining an SVG file into a DOM tree by dropping the XML declaration from the
 * beginning and returning the rest.
 */
public abstract class InlineSvgMacro extends XmlProcessingMacro {

    @Override
    protected Class<?> getType() {
        return String.class;
    }

    @Override
    protected void verifyArguments(CompilationContext compilationContext, Position position, List<Class<?>> args) {
        if (args.size() != 1 || !CompilationContext.isAssignableTo(args.getFirst(), String.class)) {
            throw new IllegalArgumentException("Expected a single String as argument.");
        }
    }

    @Override
    public void verify(CompilationContext context, Position position, List<Node> args) {
        super.verify(context, position, args);

        if (args.getFirst().isConstant()) {
            verifyPath(context, position, String.valueOf(args.getFirst().getConstantValue()));
        }
    }

    @Override
    public boolean isConstant(CompilationContext context, List<Node> args) {
        return args.getFirst().isConstant();
    }

    @Override
    public Object invoke(Environment environment, Object[] args) {
        return processResource((String) args[0]);
    }

    @Nonnull
    private String processResource(String path) {
        return stringifyElement(getSvgDocument(path).getDocumentElement(), false);
    }

    /**
     * Verifies the path argument.
     *
     * @param context  the compilation context which can be used to emit an error or warning
     * @param position the position to use for the generated errors or warnings
     * @param path     the path argument value
     */
    protected abstract void verifyPath(CompilationContext context, Position position, String path);

    /**
     * Retrieves the SVG document from the given path.
     *
     * @param path the path of the SVG document
     * @return the XML document for the path
     */
    protected abstract Document getSvgDocument(String path);
}
