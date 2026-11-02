/*
 * Made with all the love in the world
 * by scireum in Stuttgart, Germany
 *
 * Copyright by scireum GmbH
 * https://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.macros;

import org.commonmark.parser.Parser;
import org.commonmark.renderer.html.HtmlRenderer;
import sirius.kernel.di.std.Register;
import sirius.kernel.tokenizer.Position;
import sirius.pasta.noodle.Environment;
import sirius.pasta.noodle.compiler.CompilationContext;
import sirius.pasta.noodle.compiler.ir.Node;

import javax.annotation.Nonnull;
import java.util.List;

/**
 * Renders the given <a href="https://commonmark.org">CommonMark</a> text as HTML.
 * <p>
 * Any HTML contained in the given text is escaped and links are sanitized, so that the result can safely be emitted
 * via <tt>i:raw</tt>. This is e.g. used to render the descriptions of the responses of a public service in the API
 * explorer just like OpenAPI tools do.
 */
@Register
public class RenderMarkdownMacro extends BasicMacro {

    private static final Parser PARSER = Parser.builder().build();
    private static final HtmlRenderer RENDERER = HtmlRenderer.builder().escapeHtml(true).sanitizeUrls(true).build();

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
    public Object invoke(Environment environment, Object[] args) {
        String markdown = (String) args[0];
        if (markdown == null || markdown.isBlank()) {
            return "";
        }
        return RENDERER.render(PARSER.parse(markdown)).trim();
    }

    @Override
    public String getDescription() {
        return "Renders the given Markdown (CommonMark) as HTML, escaping any HTML contained in it.";
    }

    @Nonnull
    @Override
    public String getName() {
        return "renderMarkdown";
    }

    @Override
    public boolean isConstant(CompilationContext context, List<Node> args) {
        return true;
    }
}
