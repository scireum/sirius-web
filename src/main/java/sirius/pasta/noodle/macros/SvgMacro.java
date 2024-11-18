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
import sirius.kernel.tokenizer.Position;
import sirius.pasta.noodle.Environment;
import sirius.pasta.noodle.compiler.CompilationContext;
import sirius.pasta.noodle.compiler.ir.Node;
import sirius.web.dispatch.SassFunction;
import sirius.web.http.MimeHelper;
import sirius.web.sass.ast.Color;
import sirius.web.sass.ast.FunctionCall;

import javax.annotation.Nonnull;
import java.nio.charset.StandardCharsets;
import java.util.Base64;
import java.util.List;
import java.util.function.Consumer;

/**
 * Provides an abstract macro which encodes an SVG file as UTF-8 string, after optionally modifying the tint color.
 * <p>
 * The tint color, if set, is used to replace occurrences of black <tt>#000000</tt>.
 */
public abstract class SvgMacro extends XmlProcessingMacro implements SassFunction {

    @Override
    protected Class<?> getType() {
        return String.class;
    }

    @Override
    protected void verifyArguments(CompilationContext compilationContext, Position position, List<Class<?>> args) {
        if (args.isEmpty() || !CompilationContext.isAssignableTo(args.get(0), String.class)) {
            throw new IllegalArgumentException("Expected at least a String as argument.");
        }
        if (args.size() == 2 && !CompilationContext.isAssignableTo(args.get(1), String.class)) {
            throw new IllegalArgumentException("If given, the second argument needs to be a String.");
        }
        if (args.size() > 2) {
            throw new IllegalArgumentException("Expected one or two String arguments.");
        }
    }

    @Override
    public void verify(CompilationContext context, Position position, List<Node> args) {
        super.verify(context, position, args);

        if (args.get(0).isConstant()) {
            verifyPath(context, position, String.valueOf(args.getFirst().getConstantValue()));
        }

        if (args.size() > 1 && args.get(1).isConstant()) {
            verifyColorValue(String.valueOf(args.get(1).getConstantValue()),
                             exception -> context.warning(position, exception.getMessage()));
        }
    }

    @Override
    public boolean isConstant(CompilationContext context, List<Node> args) {
        return args.get(0).isConstant() && (args.size() == 1 || args.get(1).isConstant());
    }

    @Override
    public Object invoke(Environment environment, Object[] args) {
        String path = (String) args[0];
        String color = args.length > 1 ? (String) args[1] : null;
        return encodeResource(path, color);
    }

    private void verifyColorValue(String color, Consumer<IllegalArgumentException> exceptionConsumer) {
        try {
            new Color(color);
        } catch (IllegalArgumentException exception) {
            exceptionConsumer.accept(exception);
        }
    }

    @Nonnull
    private String encodeResource(String path, String color) {
        Document document = getSvgDocument(path);
        Element root = cleanIndentationAndNewlines(document.getDocumentElement());
        String svgCode = stringifyElement(root, false);

        // optionally replace black with given tint color
        if (Strings.isFilled(color)) {
            verifyColorValue(color, ignored -> {
                throw new IllegalArgumentException("Unknown color: " + color + " (Hex-color #rrggbb expected.)");
            });
            svgCode = svgCode.replace("#000000", color);
        }

        // we still need to base64-encode the data, for compatibility with IE11; more modern browsers understand utf8-
        // encoded strings directly, but require at least '#' to be replaced by '%23'
        String base64Data = Base64.getEncoder().encodeToString(svgCode.getBytes(StandardCharsets.UTF_8));
        return "data:" + MimeHelper.guessMimeType(path) + ";base64," + base64Data;
    }

    @Override
    public String eval(FunctionCall call) {
        String path = call.getExpectedParam(0).toString();
        if (path.startsWith("'") && path.endsWith("'")) {
            path = path.substring(1, path.length() - 1);
        }

        String color = call.getParameters().size() > 1 ? call.getParameters().get(1).toString() : null;

        return encodeResource(path, color);
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
