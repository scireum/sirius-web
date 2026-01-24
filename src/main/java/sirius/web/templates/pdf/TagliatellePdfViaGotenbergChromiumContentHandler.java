/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates.pdf;

import com.fasterxml.jackson.databind.node.ObjectNode;
import org.apache.hc.client5.http.classic.methods.HttpPost;
import org.apache.hc.client5.http.entity.mime.HttpMultipartMode;
import org.apache.hc.client5.http.entity.mime.MultipartEntityBuilder;
import org.apache.hc.client5.http.impl.classic.CloseableHttpClient;
import org.apache.hc.client5.http.impl.classic.HttpClients;
import org.apache.hc.core5.http.ContentType;
import org.apache.hc.core5.http.HttpEntity;
import sirius.kernel.commons.Explain;
import sirius.kernel.commons.Json;
import sirius.kernel.di.std.ConfigValue;
import sirius.kernel.di.std.Part;
import sirius.kernel.di.std.Register;
import sirius.pasta.tagliatelle.Tagliatelle;
import sirius.pasta.tagliatelle.Template;
import sirius.web.templates.ContentHandler;
import sirius.web.templates.Generator;
import sirius.web.templates.TagliatelleContentHandler;

import java.io.IOException;
import java.io.OutputStream;
import java.net.URI;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;

/**
 * Generates PDF output by evaluating a given <em>Tagliatelle</em> template producing HTML.
 * <p>
 * This handler expects <em>Tagliatelle</em> as template language which must generate valid HTML output. This HTML is
 * then post-processed by <em>Chromium</em> via <em>Gotenberg</em> to generate a PDF file. The name of this handler is
 * <b>{@value #PDF_PASTA}</b> and the file extension should be <b>.pdf.pasta</b>.
 * <h6>Running Gotenberg</h6>
 * To use this handler, a running instance of <em><a href="https://gotenberg.dev/">Gotenberg</a></em> must be available.
 * You can run Gotenberg using <em>Docker</em> by adding a service like this to your <em>docker-compose.yml</em>:
 * <pre>gotenberg:
 * image: gotenberg/gotenberg:8
 * ports:
 *   - "3000:3000"
 * hostname: gotenberg</pre>
 * <h6>Metadata</h6>
 * Various metadata fields such as author, title, subject, etc. can be set for the generated PDF file. These can either
 * be defined via configuration values or can be overridden on a per-generation basis by setting specific
 * {@linkplain Generator#getContext() context} values:
 * <ul>
 *     <li><b>{@link #KEY_METADATA_AUTHOR}</b>: Overrides the author string
 *     <li><b>{@link #KEY_METADATA_CREATOR}</b>: Overrides the creator metadata
 *     <li><b>{@link #KEY_METADATA_PRODUCER}</b>: Overrides the producer metadata
 *     <li><b>{@link #KEY_METADATA_TITLE}</b>: Overrides the title metadata
 *     <li><b>{@link #KEY_METADATA_SUBJECT}</b>: Overrides the subject
 * </ul>
 * <h6>Page Size and Margins</h6>
 * The page size and margins can also be configured globally or overridden on a per-generation basis by setting specific
 * {@linkplain Generator#getContext() context} values:
 * <ul>
 *     <li><b>{@link #KEY_PAGE_WIDTH}</b>: Overrides the page width
 *     <li><b>{@link #KEY_PAGE_HEIGHT}</b>: Overrides the page height
 *     <li><b>{@link #KEY_PAGE_MARGIN_TOP}</b>: Overrides the top
 *     <li><b>{@link #KEY_PAGE_MARGIN_BOTTOM}</b>: Overrides the bottom margin
 *     <li><b>{@link #KEY_PAGE_MARGIN_LEFT}</b>: Overrides the left
 *     <li><b>{@link #KEY_PAGE_MARGIN_RIGHT}</b>: Overrides the right margin
 * </ul>
 * <h6>Note for Developer Systems</h6>
 * Typically, a template file will use images. Make sure that such resources are either given inline as data blobs or
 * using fully qualified URLs containing a real host name or IP. Gotenberg runs in a separate container and will not be
 * able to resolve <em>localhost</em> or <em>127.0.0.1</em> to your development system.
 *
 * @see <a href="https://gotenberg.dev/docs/routes">Gotenberg API</a>
 */
@Register(name = TagliatellePdfViaGotenbergChromiumContentHandler.PDF_PASTA, classes = ContentHandler.class)
public class TagliatellePdfViaGotenbergChromiumContentHandler extends TagliatelleContentHandler {

    /**
     * Contains the name (type) of this handler
     */
    public static final String PDF_PASTA = "pdf-pasta-gotenberg-chromium";

    /**
     * {@linkplain Generator#getContext() Context} key to override the PDF file's author information. If the context
     * contains a value for this key, it is used instead of the configured default.
     *
     * @see <a href="https://gotenberg.dev/docs/routes#metadata-chromium">Gotenberg API documentation on metadata</a>
     */
    public static final String KEY_METADATA_AUTHOR = "gotenberg.metadata.author";

    /**
     * {@linkplain Generator#getContext() Context} key to override the PDF file's creator information. If the context
     * contains a value for this key, it is used instead of the configured default.
     *
     * @see <a href="https://gotenberg.dev/docs/routes#metadata-chromium">Gotenberg API documentation on metadata</a>
     */
    public static final String KEY_METADATA_CREATOR = "gotenberg.metadata.creator";

    /**
     * {@linkplain Generator#getContext() Context} key to override the PDF file's producer information. If the context
     * contains a value for this key, it is used instead of the configured default.
     *
     * @see <a href="https://gotenberg.dev/docs/routes#metadata-chromium">Gotenberg API documentation on metadata</a>
     */
    public static final String KEY_METADATA_PRODUCER = "gotenberg.metadata.producer";

    /**
     * {@linkplain Generator#getContext() Context} key to override the PDF file's title information. If the context
     * contains a value for this key, it is used instead of the configured default.
     *
     * @see <a href="https://gotenberg.dev/docs/routes#metadata-chromium">Gotenberg API documentation on metadata</a>
     */
    public static final String KEY_METADATA_TITLE = "gotenberg.metadata.title";

    /**
     * {@linkplain Generator#getContext() Context} key to override the PDF file's subject information. If the context
     * contains a value for this key, it is used instead of the configured default.
     *
     * @see <a href="https://gotenberg.dev/docs/routes#metadata-chromium">Gotenberg API documentation on metadata</a>
     */
    public static final String KEY_METADATA_SUBJECT = "gotenberg.metadata.subject";

    /**
     * {@linkplain Generator#getContext() Context} key to override the page width. The value should contain a valid CSS
     * size including a unit (e.g. "8.5in" or "210mm"). If the unit is omitted, the default unit is inches. If the
     * context contains a value for this key, it is used instead of the configured default.
     *
     * @see <a href="https://gotenberg.dev/docs/routes#page-properties-chromium">Gotenberg API documentation on page properties</a>
     */
    public static final String KEY_PAGE_WIDTH = "gotenberg.page.width";

    /**
     * {@linkplain Generator#getContext() Context} key to override the page height. The value should contain a valid CSS
     * size including a unit (e.g. "11in" or "297mm"). If the unit is omitted, the default unit is inches. If the
     * context contains a value for this key, it is used instead of the configured default.
     *
     * @see <a href="https://gotenberg.dev/docs/routes#page-properties-chromium">Gotenberg API documentation on page properties</a>
     */
    public static final String KEY_PAGE_HEIGHT = "gotenberg.page.height";

    /**
     * {@linkplain Generator#getContext() Context} key to override the top margin. The value should contain a valid CSS
     * size including a unit (e.g. "11in" or "297mm"). If the unit is omitted, the default unit is inches. If the
     * context contains a value for this key, it is used instead of the configured default.
     *
     * @see <a href="https://gotenberg.dev/docs/routes#page-properties-chromium">Gotenberg API documentation on page properties</a>
     */
    public static final String KEY_PAGE_MARGIN_TOP = "gotenberg.page.marginTop";

    /**
     * {@linkplain Generator#getContext() Context} key to override the bottom margin. The value should contain a valid CSS
     * size including a unit (e.g. "11in" or "297mm"). If the unit is omitted, the default unit is inches. If the
     * context contains a value for this key, it is used instead of the configured default.
     *
     * @see <a href="https://gotenberg.dev/docs/routes#page-properties-chromium">Gotenberg API documentation on page properties</a>
     */
    public static final String KEY_PAGE_MARGIN_BOTTOM = "gotenberg.page.marginBottom";

    /**
     * {@linkplain Generator#getContext() Context} key to override the left margin. The value should contain a valid CSS
     * size including a unit (e.g. "11in" or "297mm"). If the unit is omitted, the default unit is inches. If the
     * context contains a value for this key, it is used instead of the configured default.
     *
     * @see <a href="https://gotenberg.dev/docs/routes#page-properties-chromium">Gotenberg API documentation on page properties</a>
     */
    public static final String KEY_PAGE_MARGIN_LEFT = "gotenberg.page.marginLeft";

    /**
     * {@linkplain Generator#getContext() Context} key to override the right margin. The value should contain a valid CSS
     * size including a unit (e.g. "11in" or "297mm"). If the unit is omitted, the default unit is inches. If the
     * context contains a value for this key, it is used instead of the configured default.
     *
     * @see <a href="https://gotenberg.dev/docs/routes#page-properties-chromium">Gotenberg API documentation on page properties</a>
     */
    public static final String KEY_PAGE_MARGIN_RIGHT = "gotenberg.page.marginRight";

    @SuppressWarnings("java:S1075")
    @Explain("The path is fixed by Gotenberg's API.")
    private static final String GOTENBERG_CHROMIUM_PATH = "/forms/chromium/convert/html";

    @Part
    private Tagliatelle tagliatelle;

    @ConfigValue("tagliatelle.gotenberg.host")
    private String gotenbergHost;

    @ConfigValue("tagliatelle.gotenberg.metadata.author")
    private String gotenbergDefaultAuthor;

    @ConfigValue("tagliatelle.gotenberg.metadata.creator")
    private String gotenbergDefaultCreator;

    @ConfigValue("tagliatelle.gotenberg.metadata.producer")
    private String gotenbergDefaultProducer;

    @ConfigValue("tagliatelle.gotenberg.metadata.title")
    private String gotenbergDefaultTitle;

    @ConfigValue("tagliatelle.gotenberg.metadata.subject")
    private String gotenbergDefaultSubject;

    @ConfigValue("tagliatelle.gotenberg.page.width")
    private String gotenbergDefaultPageWidth;

    @ConfigValue("tagliatelle.gotenberg.page.height")
    private String gotenbergDefaultPageHeight;

    @ConfigValue("tagliatelle.gotenberg.page.marginTop")
    private String gotenbergDefaultMarginTop;

    @ConfigValue("tagliatelle.gotenberg.page.marginBottom")
    private String gotenbergDefaultMarginBottom;

    @ConfigValue("tagliatelle.gotenberg.page.marginLeft")
    private String gotenbergDefaultMarginLeft;

    @ConfigValue("tagliatelle.gotenberg.page.marginRight")
    private String gotenbergDefaultMarginRight;

    @Override
    public boolean generate(Generator generator, OutputStream out) throws Exception {
        if (!PDF_PASTA.equals(generator.getHandlerType())) {
            return false;
        }

        Template template = getTemplate(generator);
        if (template == null) {
            return false;
        }

        String content = template.renderWithParams(generator.getContext());

        // create metadata for Gotenberg
        ObjectNode metadata = Json.createObject()
                                  .put("Author",
                                       generator.getContext()
                                                .getValue(KEY_METADATA_AUTHOR)
                                                .asOptionalString()
                                                .orElse(gotenbergDefaultAuthor))
                                  .put("Creator",
                                       generator.getContext()
                                                .getValue(KEY_METADATA_CREATOR)
                                                .asOptionalString()
                                                .orElse(gotenbergDefaultCreator))
                                  .put("Producer",
                                       generator.getContext()
                                                .getValue(KEY_METADATA_PRODUCER)
                                                .asOptionalString()
                                                .orElse(gotenbergDefaultProducer))
                                  .put("Title",
                                       generator.getContext()
                                                .getValue(KEY_METADATA_TITLE)
                                                .asOptionalString()
                                                .orElse(gotenbergDefaultTitle))
                                  .put("Subject",
                                       generator.getContext()
                                                .getValue(KEY_METADATA_SUBJECT)
                                                .asOptionalString()
                                                .orElse(gotenbergDefaultSubject));

        // wrap the content into a multipart form data request for Gotenberg
        Charset charset = StandardCharsets.UTF_8;
        HttpEntity multipartEntity = MultipartEntityBuilder.create()
                                                           .setMode(HttpMultipartMode.LEGACY)
                                                           .setCharset(charset)
                                                           .addBinaryBody("files",
                                                                          content.getBytes(charset),
                                                                          ContentType.TEXT_HTML,
                                                                          "index.html")
                                                           .addTextBody("metadata", Json.write(metadata))
                                                           .addTextBody("generateDocumentOutline", "true")
                                                           .addTextBody("generateTaggedPdf", "true")
                                                           .addTextBody("preferCssPageSize", "true")
                                                           .addTextBody("paperWidth",
                                                                        generator.getContext()
                                                                                 .getValue(KEY_PAGE_WIDTH)
                                                                                 .asOptionalString()
                                                                                 .orElse(gotenbergDefaultPageWidth))
                                                           .addTextBody("paperHeight",
                                                                        generator.getContext()
                                                                                 .getValue(KEY_PAGE_HEIGHT)
                                                                                 .asOptionalString()
                                                                                 .orElse(gotenbergDefaultPageHeight))
                                                           .addTextBody("marginTop",
                                                                        generator.getContext()
                                                                                 .getValue(KEY_PAGE_MARGIN_TOP)
                                                                                 .asOptionalString()
                                                                                 .orElse(gotenbergDefaultMarginTop))
                                                           .addTextBody("marginBottom",
                                                                        generator.getContext()
                                                                                 .getValue(KEY_PAGE_MARGIN_BOTTOM)
                                                                                 .asOptionalString()
                                                                                 .orElse(gotenbergDefaultMarginBottom))
                                                           .addTextBody("marginLeft",
                                                                        generator.getContext()
                                                                                 .getValue(KEY_PAGE_MARGIN_LEFT)
                                                                                 .asOptionalString()
                                                                                 .orElse(gotenbergDefaultMarginLeft))
                                                           .addTextBody("marginRight",
                                                                        generator.getContext()
                                                                                 .getValue(KEY_PAGE_MARGIN_RIGHT)
                                                                                 .asOptionalString()
                                                                                 .orElse(gotenbergDefaultMarginRight))
                                                           .build();

        HttpPost postRequest = new HttpPost(URI.create(gotenbergHost + GOTENBERG_CHROMIUM_PATH));
        postRequest.setEntity(multipartEntity);

        try (CloseableHttpClient httpClient = HttpClients.createDefault()) {
            httpClient.execute(postRequest, httpResponse -> {
                if (httpResponse.getCode() != 200) {
                    throw new IOException("Gotenberg responded with HTTP " + httpResponse.getCode());
                }
                httpResponse.getEntity().writeTo(out);
                return httpResponse;
            });
        }

        out.close();
        return true;
    }

    @Override
    public int getPriority() {
        return DEFAULT_PRIORITY;
    }
}
