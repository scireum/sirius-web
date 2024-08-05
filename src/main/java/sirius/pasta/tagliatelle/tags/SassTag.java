/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.tagliatelle.tags;

import sirius.kernel.commons.Files;
import sirius.kernel.di.std.Register;
import sirius.kernel.tokenizer.ParseException;
import sirius.pasta.tagliatelle.TemplateArgument;
import sirius.pasta.tagliatelle.emitter.CompositeEmitter;
import sirius.pasta.tagliatelle.emitter.ConstantEmitter;
import sirius.web.sass.Generator;
import sirius.web.sass.Parser;
import sirius.web.sass.ast.Stylesheet;

import javax.annotation.Nonnull;
import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.util.Collections;
import java.util.List;

/**
 * Handles <tt>i:sass</tt> which renders a SASS file into the current template as compiled CSS.
 */
public class SassTag extends TagHandler {

    private static final String SASS_ELEMENT = "i:sass";

    private static final String SOURCE_ATTRIBUTE = "source";

    /**
     * Creates new tags of type {@value #SASS_ELEMENT}.
     */
    @Register
    public static class Factory implements TagHandlerFactory {

        @Nonnull
        @Override
        public String getName() {
            return SASS_ELEMENT;
        }

        @Override
        public TagHandler createHandler() {
            return new SassTag();
        }

        @Override
        public List<TemplateArgument> reportArguments() {
            return Collections.singletonList(new TemplateArgument(String.class,
                                                                  SOURCE_ATTRIBUTE,
                                                                  "Contains the path of the SASS file to render."));
        }

        @Override
        public String getDescription() {
            return "Renders a SASS file into the current template as compiled CSS.";
        }
    }

    @Override
    public void apply(CompositeEmitter targetBlock) {
        tryResolveAssetResource(getConstantAttribute(SOURCE_ATTRIBUTE).asString()).ifPresent(resource -> {
            try (Reader reader = new StringReader(resource.getContentAsString())) {
                Parser parser = new Parser(Files.getFilenameAndExtension(resource.getPath()), reader);
                Stylesheet stylesheet = parser.parse();

                Generator generator = new Generator();
                generator.importStylesheet(stylesheet);
                generator.compile();

                targetBlock.addChild(new ConstantEmitter(getStartOfTag()).append(generator.toString()));
            } catch (IOException | ParseException exception) {
                getCompilationContext().error(getStartOfTag(),
                                              "Cannot access the resource: %s (%s)",
                                              resource.getPath(),
                                              exception.getMessage());
            }
        });
    }

    @Override
    public Class<?> getExpectedAttributeType(String name) {
        if (SOURCE_ATTRIBUTE.equals(name)) {
            return String.class;
        }

        return super.getExpectedAttributeType(name);
    }
}
