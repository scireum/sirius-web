/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates;

import sirius.kernel.async.CallContext;
import sirius.kernel.commons.Context;
import sirius.kernel.commons.Explain;
import sirius.kernel.commons.Strings;
import sirius.kernel.di.GlobalContext;
import sirius.kernel.di.std.Part;
import sirius.kernel.di.std.PriorityParts;
import sirius.kernel.di.std.Register;
import sirius.kernel.health.Exceptions;
import sirius.kernel.health.HandledException;
import sirius.web.http.MimeHelper;
import sirius.web.mails.Attachment;
import sirius.web.mails.BufferedAttachment;
import sirius.web.mails.MailSender;
import sirius.web.resources.Resolver;
import sirius.web.resources.Resource;
import sirius.web.resources.Resources;

import javax.activation.DataSource;
import javax.annotation.Nonnull;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.Collection;

/**
 * Used to generate content by either evaluating a template or directly supplied template code.
 * <p>
 * This uses a builder like pattern (a.k.a. fluent API) and requires to either call {@link #generate()} or
 * {@link #generateTo(OutputStream)} to finally generate the content.
 */
public class Generator {

    /**
     * If a specific output encoding is required (other than the system encoding - most definitely UTF-8) a variable
     * using this key can be supplied to the generator, specifying the name of the encoding to use.
     * <p>
     * If possible however it is preferable to use {@link Generator#encoding(String)} to set the encoding.
     */
    public static final String ENCODING = "encoding";

    @PriorityParts(ContentHandler.class)
    private static Collection<ContentHandler> handlers;

    @Part
    private static GlobalContext globalContext;

    @Part
    private static Resources resources;

    @Part
    private static Templates templates;

    private String templateName;
    private String templateCode;
    private String handlerType;
    private final Context context = Context.create();
    private String encoding;

    protected Generator() {
    }

    /**
     * Applies the context to the generator.
     * <p>
     * This will join the given context with the one previously set (Or the system context). All values with
     * the same name will be overwritten using the values in the given context.
     *
     * @param ctx the context to be applied to the one already present
     * @return the generator itself for fluent API calls
     */
    public Generator applyContext(Context ctx) {
        context.putAll(ctx);
        return this;
    }

    /**
     * Adds a variable with the given name (key) and value to the internal context.
     * <p>
     * If a value with the same key was already defined, it will be overwritten.
     *
     * @param key   the name of the variable to set
     * @param value the value of the variable to set
     * @return the generator itself for fluent API calls
     */
    public Generator put(String key, Object value) {
        context.put(key, value);
        return this;
    }

    /**
     * Sets the output encoding which is used to generate the output files.
     *
     * @param encoding the encoding to use for output files
     * @return the generator itself for fluent API calls
     */
    public Generator encoding(String encoding) {
        this.encoding = encoding;
        return this;
    }

    /**
     * Determines which template file should be used.
     * <p>
     * The content is resolved by calling {@link Resources#resolve(String)}.
     *
     * @param templateName the name of the template to use
     * @return the generator itself for fluent API calls
     */
    public Generator useTemplate(String templateName) {
        this.templateName = templateName;
        return this;
    }

    /**
     * Sets the template code to be used directly as string.
     * <p>
     * Most probably this will be velocity code. Once a direct code is set, the template specified by
     * {@link #useTemplate(String)} will be ignored.
     *
     * @param templateCode the template code to evaluate
     * @param handlerType  String reference for the handler to be used
     *                     (i.e. {@link TagliatelleContentHandler#PASTA})
     * @return the generator itself for fluent API calls
     */
    public Generator direct(String templateCode, String handlerType) {
        this.templateCode = templateCode;
        handler(handlerType);
        return this;
    }

    /**
     * Specifies which {@link ContentHandler} is used to generate the content.
     * <p>
     * Most of the time, the content handler is auto-detected using the file name of the template. An example
     * would be <b>.pdf.pasta</b> which will force the {@link sirius.web.templates.pdf.TagliatellePDFContentHandler}
     * to generate a PDF file using the template. However, by using {@code generator.handler("pdf-pasta")}
     * it can be ensured, that this handler is picked, without relying on the file name.
     *
     * @param handlerType the name of the handler type to use. Constants can be found by looking at the
     *                    {@link Register} annotations of the implementing classes of {@link ContentHandler}.
     * @return the generator itself for fluent API calls
     */
    public Generator handler(String handlerType) {
        this.handlerType = handlerType;
        return this;
    }

    /**
     * Calls the appropriate {@link ContentHandler} to generate the output which is written into the given
     * output stream.
     *
     * @param out the output stream to which the generated content is written
     */
    public void generateTo(OutputStream out) {
        if (Strings.isFilled(templateName)) {
            CallContext.getCurrent().addToMDC("content-generator-template", templateName);
        }
        try {
            try {
                if (Strings.isFilled(handlerType)) {
                    generateContentUsingHandler(out);
                } else {
                    findAndInvokeContentHandler(out);
                }
            } catch (HandledException e) {
                throw e;
            } catch (Exception e) {
                throw Exceptions.handle()
                                .error(e)
                                .to(Templates.LOG)
                                .withSystemErrorMessage("Error applying template '%s': %s (%s)",
                                                        Strings.isEmpty(templateName) ? templateCode : templateName)
                                .handle();
            }
        } finally {
            CallContext.getCurrent().removeFromMDC("content-generator-template");
        }
    }

    /**
     * Convenience method to directly create an {@link Attachment} which can be added to an email.
     * <p>
     * Internally a buffer is created and {@link #generateTo(OutputStream)} is invoked to generate
     * the actual content of the attachment.
     *
     * @param filename the filename of the attachment.
     * @return the attachment which can be passed to
     * {@link MailSender#addAttachment(DataSource)}.
     */
    public Attachment generateAttachment(@Nonnull String filename) {
        ByteArrayOutputStream buffer = new ByteArrayOutputStream();
        generateTo(buffer);

        return new BufferedAttachment(filename, MimeHelper.guessMimeType(filename), buffer.toByteArray(), false);
    }

    private void findAndInvokeContentHandler(OutputStream out) throws Exception {
        for (ContentHandler handler : handlers) {
            if (handler.generate(this, out)) {
                return;
            }
        }
        throw Exceptions.handle()
                        .to(Templates.LOG)
                        .withSystemErrorMessage("No handler was able to render the given template: %s",
                                                Strings.isEmpty(templateName) ? templateCode : templateName)
                        .handle();
    }

    private void generateContentUsingHandler(OutputStream out) throws Exception {
        ContentHandler handler = globalContext.findPart(handlerType, ContentHandler.class);
        if (!handler.generate(this, out)) {
            throw Exceptions.handle()
                            .to(Templates.LOG)
                            .withSystemErrorMessage("Error using '%s' to generate template '%s'.",
                                                    handlerType,
                                                    Strings.isEmpty(templateName) ? templateCode : templateName)
                            .handle();
        }
    }

    /**
     * Invokes the appropriate {@link ContentHandler} and returns the generated content handler as string.
     * <p>
     * Most probably the input will be a velocity template which generates readable text (which also might be
     * XML or HTML).
     *
     * @return the generated string contents
     */
    public String generate() {
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        generateTo(out);
        return out.toString(StandardCharsets.UTF_8);
    }

    /**
     * Can be used by a {@link ContentHandler} to obtain a preset templateName.
     *
     * @return the templateName which was previously set or <tt>null</tt> if no template name was given
     */
    public String getTemplateName() {
        return templateName;
    }

    /**
     * Can be used by a {@link ContentHandler} to obtain a preset templateCode.
     *
     * @return the templateCode which was previously set or <tt>null</tt> if no template code was given
     */
    public String getTemplateCode() {
        return templateCode;
    }

    /**
     * Can be used by a {@link ContentHandler} to access the context which contains all previously set variables.
     *
     * @return the previously set context containing all applied variables.
     */
    @SuppressWarnings("AssignmentOrReturnOfFieldWithMutableType")
    @Explain("We intentionally return a mutable reference here as the context can be extended")
    public Context getContext() {
        return context;
    }

    /**
     * Can be used by a {@link ContentHandler} to determine the file ending of the selected template. This is
     * used to select which content handler is actually used to generate the output.
     *
     * @param extension the expected file extension, without a "." at the beginning
     * @return <tt>true</tt> if the given template ends with the given extension, <tt>false</tt> otherwise. This
     * first dot is considered the start of the file extension so "foobar.test.js" has "test.js" as extension.
     * If the templateName is <tt>null</tt>, this method always returns <tt>false</tt>.
     */
    public boolean isTemplateFileExtension(@Nonnull String extension) {
        return Strings.isFilled(templateName) && extension.equalsIgnoreCase(Strings.split(templateName, ".")
                                                                                   .getSecond());
    }

    /**
     * Can be used by a {@link ContentHandler} to determine the effective ending of the underlying template name.
     *
     * @param extension the expected end of the file name.
     * @return <tt>true</tt> if the given template ends with the given extension, <tt>false</tt> otherwise.
     * In contrast to {@link #isTemplateFileExtension(String)} this will not consider the first "." to be the
     * file extension but rather really check if the template name ends with the given extension. Therefore
     * for a template named <tt>test.js.pasta</tt> this will return <tt>true</tt> for
     * {@code isTemplateEndsWith(".pasta")} but <tt>false</tt> for {@code isTemplateFileExtension("pasta")}
     */
    public boolean isTemplateEndsWith(@Nonnull String extension) {
        return Strings.isFilled(templateName) && templateName.toLowerCase().endsWith(extension);
    }

    /**
     * Can be used by a {@link ContentHandler} to determine the effective encoding used for the generated output.
     * This is either set via {@link #encoding(String)} or by placing a variable named {@link #ENCODING} in the
     * context or it is the default encoding used by the JVM (most probably UTF-8).
     *
     * @return the effective encoding used to generate the output
     */
    public String getEncoding() {
        if (Strings.isFilled(encoding)) {
            return encoding;
        }
        if (context.containsKey(ENCODING)) {
            return (String) context.get(ENCODING);
        }
        return StandardCharsets.UTF_8.name();
    }

    /**
     * Contains the handler type. This can be used by a {@link ContentHandler} to skip all filename checks and
     * always generate its output.
     *
     * @return the handlerType previously set using {@link #handler(String)} or <tt>null</tt> if no handler type
     * was set.
     */
    public String getHandlerType() {
        return handlerType;
    }

    /**
     * Uses the {@link Resolver} implementations or the classloader to load the template as input stream.
     *
     * @return the contents of the template as stream or <tt>null</tt> if the template cannot be resolved
     */
    public InputStream getTemplate() {
        try {
            if (templateName == null) {
                throw Exceptions.handle()
                                .to(Templates.LOG)
                                .withSystemErrorMessage("No template was given to evaluate.")
                                .handle();
            }
            URL url = resources.resolve(templateName).map(Resource::getUrl).orElse(null);
            if (url == null) {
                throw Exceptions.handle()
                                .to(Templates.LOG)
                                .withSystemErrorMessage("Unable to resolve '%s'", templateName)
                                .handle();
            }
            return url.openStream();
        } catch (IOException e) {
            throw Exceptions.handle(Templates.LOG, e);
        }
    }
}
