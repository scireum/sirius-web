/*
 * Made with all the love in the world
 * by scireum in Stuttgart, Germany
 *
 * Copyright by scireum GmbH
 * https://www.scireum.de - info@scireum.de
 */

package sirius.web.mails;

import jakarta.activation.DataSource;
import sirius.kernel.commons.Strings;
import sirius.kernel.di.std.Part;
import sirius.kernel.health.Exceptions;
import sirius.kernel.health.Log;
import sirius.kernel.nls.Formatter;
import sirius.web.security.oauth.OAuth;
import sirius.web.security.oauth.OAuthTokenProviderUtils;
import sirius.web.services.JSONCall;
import sirius.web.services.JSONStructuredOutput;

import java.io.IOException;
import java.net.URI;
import java.util.ArrayList;
import java.util.Base64;
import java.util.List;

/**
 * Represents a mail to be sent via the Microsoft Graph API.
 * <p>
 * Instead of using the standard SMTP protocol, the Microsoft Graph API (using HTTP POST requests containing a JSON
 * body) is used to send emails as SMTP is considered legacy and only available for certain Microsoft plans.
 */
public class MicrosoftGraphApiMail {

    @Part
    private static OAuthTokenProviderUtils oauthTokenProviderUtils;

    private static final Log LOG = Log.get("microsoft-graph-api-mail");

    private URI endpoint;
    private String subject;
    private String receiverMailAddress;
    private String oauthTokenName;
    private boolean saveToSentItems;

    private String bodyContent;
    private String bodyType;

    private final List<Attachment> attachments = new ArrayList<>();

    /**
     * Creates a new instance of {@link MicrosoftGraphApiMail} based on the provided {@link MailSender}.
     * <p>
     * Note, that the HTML body is preferred over the plain text body as only a single body type can be defined.
     *
     * @param mail            the {@link MailSender} containing the data and configuration for the mail to be sent
     * @param endpoint        the endpoint URI for the Microsoft Graph API mail service
     * @param saveToSentItems whether the mail should be saved to the "Sent Items" folder
     * @return a new instance of {@link MicrosoftGraphApiMail} configured with the details from the {@link MailSender}
     */
    public static MicrosoftGraphApiMail createFromMail(MailSender mail, URI endpoint, boolean saveToSentItems) {
        MicrosoftGraphApiMail microsoftGraphApiMail =
                create().withOAuthTokenName(mail.getSmtpConfiguration().getOAuthTokenName())
                        .withEndpoint(endpoint)
                        .withReceiverEmailAddress(mail.getReceiverName())
                        .withSubject(mail.getSubject())
                        .withSaveToSentItems(saveToSentItems);

        if (Strings.isFilled(mail.getHtml())) {
            microsoftGraphApiMail.withHtmlBody(mail.getHtml());
        } else {
            microsoftGraphApiMail.withTextBody(mail.getText());
        }

        mail.getAttachments().forEach(microsoftGraphApiMail::addAttachment);

        return microsoftGraphApiMail;
    }

    /**
     * Creates a new instance of {@link MicrosoftGraphApiMail}.
     *
     * @return a new instance of {@link MicrosoftGraphApiMail}
     */
    public static MicrosoftGraphApiMail create() {
        return new MicrosoftGraphApiMail();
    }

    /**
     * Sets the name of the OAuth token to be used for authentication.
     * <p>
     * The access token itself will then be fetched via the {@link OAuthTokenProviderUtils} using the given name.
     *
     * @param oauthTokenName the name of the OAuth token to be used for authentication
     * @return the current instance of {@link MicrosoftGraphApiMail} for method chaining
     */
    public MicrosoftGraphApiMail withOAuthTokenName(String oauthTokenName) {
        this.oauthTokenName = oauthTokenName;
        return this;
    }

    /**
     * Sets the endpoint for the Microsoft Graph API mail service.
     *
     * @param endpoint the endpoint URI for the Microsoft Graph API mail service
     * @return the current instance of {@link MicrosoftGraphApiMail} for method chaining
     */
    public MicrosoftGraphApiMail withEndpoint(URI endpoint) {
        this.endpoint = endpoint;
        return this;
    }

    /**
     * Sets the subject of the mail to be sent.
     *
     * @param subject the subject of the mail
     * @return the current instance of {@link MicrosoftGraphApiMail} for method chaining
     */
    public MicrosoftGraphApiMail withSubject(String subject) {
        this.subject = subject;
        return this;
    }

    /**
     * Sets the email address of the receiver.
     *
     * @param receiverMailAddress the email address of the receiver
     * @return the current instance of {@link MicrosoftGraphApiMail} for method chaining
     */
    public MicrosoftGraphApiMail withReceiverEmailAddress(String receiverMailAddress) {
        this.receiverMailAddress = receiverMailAddress;
        return this;
    }

    /**
     * Sets whether the mail should be saved to the "Sent Items" folder.
     *
     * @param saveToSentItems <tt>true</tt> if the mail should be saved to "Sent Items", <tt>false</tt> otherwise
     * @return the current instance of {@link MicrosoftGraphApiMail} for method chaining
     */
    public MicrosoftGraphApiMail withSaveToSentItems(boolean saveToSentItems) {
        this.saveToSentItems = saveToSentItems;
        return this;
    }

    /**
     * Sets the body of the mail to be sent as HTML.
     * <p>
     * Note, that only one type of body (HTML or plain text) can be defined for a single mail. If an HTML body is set,
     * Microsoft Graph API will create a plain text counterpart automatically based on the HTML content.
     *
     * @param body the HTML content of the mail body
     * @return the current instance of {@link MicrosoftGraphApiMail} for method chaining
     */
    public MicrosoftGraphApiMail withHtmlBody(String body) {
        this.bodyContent = body;
        this.bodyType = "html";
        return this;
    }

    /**
     * Sets the body of the mail to be sent as plain text.
     *
     * @param body the plain text content of the mail body
     * @return the current instance of {@link MicrosoftGraphApiMail} for method chaining
     * @see #withHtmlBody(String) for details concerning how Microsoft Graph API handles the body content
     */
    public MicrosoftGraphApiMail withTextBody(String body) {
        this.bodyContent = body;
        this.bodyType = "text";
        return this;
    }

    /**
     * Adds an attachment to the mail being sent.
     *
     * @param dataSource the data source representing the attachment to be added
     * @return the current instance of {@link MicrosoftGraphApiMail} for method chaining
     */
    public MicrosoftGraphApiMail addAttachment(DataSource dataSource) {
        this.attachments.add(Attachment.fromDataSource(dataSource));
        return this;
    }

    /**
     * Sends the mail using the Microsoft Graph API.
     *
     * @throws IOException if an error occurs while sending the mail or processing the response
     * @implNote Microsoft Graph API responses with just a HTTP status code and no body content. Thus, the JSON call
     * requires allowing an empty response body
     */
    public void send() throws IOException {
        assertValidConfiguration();

        JSONCall call = JSONCall.to(endpoint).withFineLogger(LOG).withAllowEmptyResponseBody(true);
        call.getOutcall().withOAuth(this::fetchValidAccessToken, this::refreshAccessToken);
        addPayload(call.getOutput());

        // Note: Invoke method to trigger JSON logging as just getting the response code skips it.
        call.getInput();

        int responseCode = call.getOutcall().getResponseCode();
        assertSuccessfulCall(responseCode);
    }

    private void assertValidConfiguration() {
        if (Strings.isEmpty(endpoint)) {
            throw new IllegalStateException("Endpoint for Microsoft Graph API mail is not set.");
        }
        if (Strings.isEmpty(subject)) {
            throw new IllegalStateException("Subject for Microsoft Graph API mail is not set.");
        }
        if (Strings.isEmpty(receiverMailAddress)) {
            throw new IllegalStateException("Receiver mail address for Microsoft Graph API mail is not set.");
        }
        if (Strings.isEmpty(oauthTokenName)) {
            throw new IllegalStateException("OAuth token name for Microsoft Graph API mail is not set.");
        }
        if (Strings.isEmpty(bodyContent) || Strings.isEmpty(bodyType)) {
            throw new IllegalStateException("Body for Microsoft Graph API mail is not set.");
        }
    }

    private String fetchValidAccessToken() {
        String token = oauthTokenProviderUtils.fetchValidTokenForCurrentScope(oauthTokenName).orElseThrow(() -> {
            return Exceptions.handle()
                             .withSystemErrorMessage(
                                     "No valid access token found for Microsoft Graph API with name '%s'.",
                                     oauthTokenName)
                             .handle();
        });

        return OAuth.TOKEN_TYPE_BEARER + " " + token;
    }

    private void refreshAccessToken() {
        // No-op: The access token is refreshed automatically by the access token provider.
    }

    private void addPayload(JSONStructuredOutput output) {
        output.beginResult();
        {
            output.beginObject("message");
            {
                addSubject(output);
                addBody(output);
                addRecipient(output);
                addAttachments(output);
            }
            output.endObject();

            output.property("saveToSentItems", saveToSentItems);
        }
        output.endResult();
    }

    private void addSubject(JSONStructuredOutput output) {
        output.property("subject", subject);
    }

    private void addBody(JSONStructuredOutput output) {
        output.beginObject("body");
        {
            output.property("contentType", bodyType);
            output.property("content", bodyContent);
        }
        output.endObject();
    }

    private void addRecipient(JSONStructuredOutput output) {
        output.beginArray("toRecipients");
        {
            output.beginObject("");
            {
                output.beginObject("emailAddress");
                {
                    output.property("address", receiverMailAddress);
                }
                output.endObject();
            }
            output.endObject();
        }
        output.endArray();
    }

    private void addAttachments(JSONStructuredOutput output) {
        if (attachments.isEmpty()) {
            return;
        }

        output.beginArray("attachments");
        {
            for (Attachment attachment : attachments) {
                output.beginObject("");
                {
                    output.property("@odata.type", "#microsoft.graph.fileAttachment");
                    output.property("name", attachment.filename());
                    output.property("contentType", attachment.contentType());
                    output.property("contentBytes", attachment.base64Content());
                }
                output.endObject();
            }
        }
        output.endArray();
    }

    /**
     * Asserts that the call to the Microsoft Graph API was successful by checking that the response code is 202.
     *
     * @param responseCode the response code received from the Microsoft Graph API
     * @implNote Microsoft Graph API does not send a body in the response, thus only the response code can be checked.
     */
    private void assertSuccessfulCall(int responseCode) {
        if (responseCode != 202) {
            throw Exceptions.handle()
                            .withSystemErrorMessage(
                                    "Failed to send mail with subject '%s' via Microsoft Graph API. HTTP status: %s",
                                    subject,
                                    responseCode)
                            .handle();
        }
    }

    private URI createEndpoint(String user, String microsoftGraphApiEndpoint) {
        return URI.create(Formatter.create(microsoftGraphApiEndpoint).set("user", user).format());
    }

    /**
     * Represents an attachment to be included in a Microsoft Graph API mail.
     *
     * @param filename      the name of the attachment file
     * @param contentType   the MIME type of the attachment
     * @param base64Content the base64 encoded content of the attachment
     */
    public record Attachment(String filename, String contentType, String base64Content) {

        /**
         * Creates an attachment from a {@link DataSource}.
         *
         * @param dataSource the data source representing the attachment
         * @return a new instance of {@link Attachment} containing the data from the data source
         */
        public static Attachment fromDataSource(DataSource dataSource) {
            return new Attachment(dataSource.getName(), dataSource.getContentType(), readAndEncodeContent(dataSource));
        }

        /**
         * Reads the content of the given {@link DataSource} and encodes it as a Base64 string.
         *
         * @param dataSource the data source to read the content from
         * @return the Base64 encoded content of the data source
         */
        private static String readAndEncodeContent(DataSource dataSource) {
            try (var inputStream = dataSource.getInputStream()) {
                byte[] bytes = inputStream.readAllBytes();
                return Base64.getEncoder().encodeToString(bytes);
            } catch (IOException exception) {
                throw Exceptions.handle(exception);
            }
        }
    }
}
