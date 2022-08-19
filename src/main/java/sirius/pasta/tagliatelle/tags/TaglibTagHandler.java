/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.tagliatelle.tags;

import sirius.kernel.commons.Strings;
import sirius.pasta.tagliatelle.Template;
import sirius.pasta.tagliatelle.TemplateArgument;
import sirius.pasta.tagliatelle.emitter.CompositeEmitter;

/**
 * Handles the invocation of a user-defined tag.
 * <p>
 * Templates named <tt>/taglib/PREFIX/tagName.html.pasta</tt> can be referenced via &lt;PREFIX:tagName&gt;. Their
 * invocation is handled by this handler.
 */
public class TaglibTagHandler extends TagHandler {

    private final Template template;

    /**
     * Creates a new handler for the given template.
     *
     * @param template the template which was resolved via a tag prefix and name.
     */
    public TaglibTagHandler(Template template) {
        this.template = template;
    }

    @Override
    public void apply(CompositeEmitter targetBlock) {
        targetBlock.addChild(getCompilationContext().invokeTemplate(getStartOfTag(),
                                                                    template,
                                                                    this::getAttribute,
                                                                    blocks));
    }

    @Override
    public Class<?> getExpectedAttributeType(String name) {
        for (TemplateArgument arg : template.getArguments()) {
            if (Strings.areEqual(arg.getName(), name)) {
                return arg.getType();
            }
        }

        return super.getExpectedAttributeType(name);
    }
}
