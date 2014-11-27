package sirius.web.templates;

import sirius.kernel.di.std.Priorized;

import java.io.OutputStream;

/**
 * Used to effectively generate the output as described by a given {@link Content.Generator}.
 * <p>
 * Handlers can be registered in the component model an will be called one after another until one feels
 * responsible to generate the effective output. The order of the handlers is defined by their
 * {@link sirius.kernel.di.std.Priorized#getPriority()} (sorted ascending).
 * </p>
 * <p>
 * Each handler should check if either the handler type  matches its own, or if the defined template has an
 * appropriate file extension.
 * </p>
 *
 * @author Andreas Haufler (aha@scireum.de)
 * @see sirius.web.templates.Content.Generator#getHandlerType()
 * @see sirius.web.templates.Content.Generator#isTemplateEndsWith(String)
 * @since 2014/02
 */
public interface ContentHandler extends Priorized {

    /**
     * Generates the appropriate output if the settings of the generator match.
     *
     * @param generator the generate settings given by the caller
     * @param out       the output stream used to write the generated content to
     * @return <tt>true</tt> if output was generated, <tt>false</tt> if the settings didn't match
     * @throws Exception if an error occurs while generating content
     */
    boolean generate(Content.Generator generator, OutputStream out) throws Exception;

}
