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
 * <b>{@value #PDF_PASTA}</b> and the expected file extension is <b>.pdf.pasta</b>.
 */
@Register(name = TagliatellePdfViaGotenbergChromiumContentHandler.PDF_PASTA, classes = ContentHandler.class)
public class TagliatellePdfViaGotenbergChromiumContentHandler extends TagliatelleContentHandler {

    /**
     * Contains the name (type) of this handler
     */
    public static final String PDF_PASTA = "pdf-pasta-gotenberg-chromium";

    @SuppressWarnings("java:S1075")
    @Explain("The path is fixed by Gotenberg's API.")
    private static final String GOTENBERG_CHROMIUM_PATH = "/forms/chromium/convert/html";

    private URI gotenbergChromiumUri;

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

    @Override
    public boolean generate(Generator generator, OutputStream out) throws Exception {
        if (!PDF_PASTA.equals(generator.getHandlerType()) && !generator.isTemplateFileExtension("pdf.pasta")) {
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
                                                .getValue("gotenberg.metadata.author")
                                                .asOptionalString()
                                                .orElse(gotenbergDefaultAuthor))
                                  .put("Creator",
                                       generator.getContext()
                                                .getValue("gotenberg.metadata.creator")
                                                .asOptionalString()
                                                .orElse(gotenbergDefaultCreator))
                                  .put("Producer",
                                       generator.getContext()
                                                .getValue("gotenberg.metadata.producer")
                                                .asOptionalString()
                                                .orElse(gotenbergDefaultProducer))
                                  .put("Title",
                                       generator.getContext()
                                                .getValue("gotenberg.metadata.title")
                                                .asOptionalString()
                                                .orElse(gotenbergDefaultTitle))
                                  .put("Subject",
                                       generator.getContext()
                                                .getValue("gotenberg.metadata.subject")
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
                                                           .build();

        if (gotenbergChromiumUri == null) {
            gotenbergChromiumUri = URI.create(gotenbergHost + GOTENBERG_CHROMIUM_PATH);
        }

        HttpPost postRequest = new HttpPost(gotenbergChromiumUri);
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
