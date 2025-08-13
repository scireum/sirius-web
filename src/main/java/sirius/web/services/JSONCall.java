/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.services;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import io.netty.handler.codec.http.HttpHeaderNames;
import sirius.kernel.commons.Json;
import sirius.kernel.commons.Streams;
import sirius.kernel.commons.Strings;
import sirius.kernel.health.Exceptions;
import sirius.kernel.health.Log;
import sirius.kernel.nls.Formatter;
import sirius.kernel.xml.Outcall;
import sirius.web.http.MimeHelper;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.util.function.BooleanSupplier;

/**
 * Simple call to send JSON to a server (URL) and receive JSON back.
 */
public class JSONCall {

    private Outcall outcall;
    private Log debugLogger = Log.get("json");
    private BooleanSupplier isDebugLogActive = () -> true;

    /*
     * Use .to(URL) to generate an instance.
     */
    private JSONCall() {
    }

    /**
     * Creates a new JSONCall for the given url with Content-Type {@link MimeHelper#APPLICATION_JSON application/json}.
     *
     * @param url the target URL to call
     * @return an <tt>JSONCall</tt> which can be used to send and receive JSON
     */
    public static JSONCall to(URI url) {
        return to(url, MimeHelper.APPLICATION_JSON + "; charset=" + StandardCharsets.UTF_8.name());
    }

    /**
     * Creates a new JSONCall for the given url.
     *
     * @param url         the target URL to call
     * @param contentType the Content-Type to use
     * @return a new instance to perform the JSON call
     */
    public static JSONCall to(URI url, String contentType) {
        JSONCall result = new JSONCall();
        result.outcall = new Outcall(url);
        result.outcall.setRequestProperty(HttpHeaderNames.CONTENT_TYPE.toString(), contentType);
        return result;
    }

    /**
     * Logs the outcall to {@code logger}.
     * <p>
     * The outcall is only logged when the logger is set to FINE. The default logger is "json".
     *
     * @param logger the logger to log to
     * @return returns the JSON call itself for fluent method calls
     */
    public JSONCall withFineLogger(Log logger) {
        this.debugLogger = logger;
        return this;
    }

    /**
     * Logs the outcall to {@code logger}.
     * <p>
     * The outcall is only logged when the logger is set to FINE. The default logger is "json".
     *
     * @param logger           the logger to log to
     * @param isDebugLogActive a supplier which returns true if the log should be written
     * @return returns the JSON call itself for fluent method calls
     */
    public JSONCall withFineLogger(Log logger, BooleanSupplier isDebugLogActive) {
        this.debugLogger = logger;
        this.isDebugLogActive = isDebugLogActive;
        return this;
    }

    /**
     * Adds a custom header field to the call
     *
     * @param name  name of the field
     * @param value value of the field
     * @return the JSON call itself for fluent method calls
     */
    public JSONCall addHeader(String name, String value) {
        outcall.setRequestProperty(name, value);
        return this;
    }

    /**
     * Can be used to generate the JSON request.
     * <p>
     * This will mark the underlying {@link Outcall} as a POST request.
     *
     * @return the input, which can be used to generate a JSON document which is sent to the URL
     * @throws IOException in case of an IO error while sending the JSON document
     */
    public JSONStructuredOutput getOutput() throws IOException {
        return new JSONStructuredOutput(outcall.postFromOutput(), null, StandardCharsets.UTF_8.name());
    }

    private void logRequest(String response) throws IOException {
        if (debugLogger != null && debugLogger.isFINE() && isDebugLogActive.getAsBoolean()) {
            debugLogger.FINE(Formatter.create("""
                                                      ---------- call ----------
                                                      ${httpMethod} ${url} [
                                                      
                                                      ${callBody}]
                                                      ---------- response ----------
                                                      HTTP-Response-Code: ${responseCode} [
                                                      
                                                      ${response}]
                                                      ---------- end ----------
                                                      """)
                                      .set("httpMethod", outcall.getRequest().method())
                                      .set("url", outcall.getRequest().uri())
                                      .set("callBody", resolveRequestBodyPretty())
                                      .set("responseCode", getOutcall().getResponseCode())
                                      .set("response", resolveResponseBodyPretty(response))
                                      .smartFormat());
        }
    }

    private String resolveRequestBodyPretty() throws IOException {
        if (outcall.getRequest().bodyPublisher().isEmpty()) {
            return null;
        }

        try (OutputStream outputStream = outcall.postFromOutput()) {
            String request = outputStream.toString();
            return Strings.isFilled(request) ? Json.writePretty(Json.parseObject(request)) : null;
        }
    }

    private String resolveResponseBodyPretty(String response) {
        if (Strings.isEmpty(response)) {
            return null;
        }

        try {
            return Json.writePretty(Json.MAPPER.readTree(response));
        } catch (JsonProcessingException exception) {
            Exceptions.ignore(exception);
            return response;
        }
    }

    /**
     * Executes the call and returns the input expecting a JSON object as a result.
     *
     * @return the result of the call as a JSON object
     * @throws IOException in case of an IO error during the call
     */
    public ObjectNode getInput() throws IOException {
        return Json.parseObject(executeCall());
    }

    /**
     * Executes the call and returns the input expecting a JSON array as a result.
     *
     * @return the result of the call as a JSON array
     * @throws IOException in case of an IO error during the call
     */
    public ArrayNode getInputArray() throws IOException {
        return Json.parseArray(executeCall());
    }

    /**
     * Executes the call and returns the input as a plain text string.
     * <p>
     * An {@link IOException} is thrown in case of an issue with the connection or if the response isn't JSON. Note
     * that non-OK responses (e.g. HTTP status 404) are accepted as long as the content type is JSON to support APIs
     * that return proper error messages in JSON format.
     *
     * @return the result of the call as a plain text string
     * @throws IOException in case of an IO error during the call
     */
    private String executeCall() throws IOException {
        String body =
                Streams.readToString(new InputStreamReader(outcall.getResponse().body(), outcall.getContentEncoding()));

        logRequest(body);

        String contentType = outcall.getHeaderField("content-type");
        if (outcall.isErroneous() && (contentType == null || !contentType.toLowerCase()
                                                                         .contains(MimeHelper.APPLICATION_JSON))) {
            throw new IOException(Strings.apply("A non-OK response (%s) was received as a result of an HTTP call",
                                                outcall.getResponse().statusCode()));
        }

        return body;
    }

    /**
     * Provides access to the underlying <tt>outcall</tt>.
     *
     * @return the underlying outcall
     */
    public Outcall getOutcall() {
        return outcall;
    }
}
