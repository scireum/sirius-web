/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.services;

import com.fasterxml.jackson.databind.node.ObjectNode;
import io.netty.handler.codec.http.HttpHeaderNames;
import sirius.kernel.commons.Json;
import sirius.kernel.commons.Streams;
import sirius.kernel.commons.Strings;
import sirius.kernel.health.Log;
import sirius.kernel.nls.Formatter;
import sirius.kernel.xml.Outcall;
import sirius.web.http.MimeHelper;

import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URI;
import java.nio.charset.StandardCharsets;

/**
 * Simple call to send JSON to a server (URL) and receive JSON back.
 */
public class JSONCall {

    private Outcall outcall;
    private Log debugLogger = Log.get("json");

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
     * @throws java.io.IOException in case of an IO error
     */
    public static JSONCall to(URI url) throws IOException {
        return to(url, MimeHelper.APPLICATION_JSON + "; charset=" + StandardCharsets.UTF_8.name());
    }

    /**
     * Creates a new JSONCall for the given url.
     *
     * @param url         the target URL to call
     * @param contentType the Content-Type to use
     * @return a new instance to perform the JSON call
     * @throws IOException in case of an IO error
     */
    public static JSONCall to(URI url, String contentType) throws IOException {
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
     * @return the input which can be used to generate a JSON document which is sent to the URL
     * @throws IOException in case of an IO error while sending the JSON document
     */
    public JSONStructuredOutput getOutput() throws IOException {
        return new JSONStructuredOutput(outcall.postFromOutput(), null, StandardCharsets.UTF_8.name());
    }

    private void logRequest(String response) throws IOException {
        if (debugLogger != null && debugLogger.isFINE()) {
            debugLogger.FINE(Formatter.create("""
                                                      ---------- call ----------
                                                      ${httpMethod} ${url} [
                                                                                   
                                                      ${callBody}]
                                                      ---------- response ----------
                                                      HTTP-Response-Code: ${responseCode}
                                                                                   
                                                      ${response}
                                                      ---------- end ----------
                                                      """)
                                      .set("httpMethod", outcall.getRequest().method())
                                      .set("url", outcall.getRequest().uri())
                                      .set("callBody",
                                           outcall.getRequest().bodyPublisher().isPresent() ? getOutput() : null)
                                      .set("responseCode", getOutcall().getResponseCode())
                                      .set("response", response)
                                      .smartFormat());
        }
    }

    /**
     * Provides access to the JSON answer of the call.
     *
     * @return the JSON result of the call
     * @throws IOException in case of an IO error while receiving the result
     */
    public ObjectNode getInput() throws IOException {
        String body =
                Streams.readToString(new InputStreamReader(outcall.getResponse().body(), outcall.getContentEncoding()));
        logRequest(body);

        String contentType = outcall.getHeaderField("content-type");
        if (!outcall.isErroneous() || (contentType != null && contentType.toLowerCase()
                                                                         .contains(MimeHelper.APPLICATION_JSON))) {
            return Json.parseObject(body);
        }
        throw new IOException(Strings.apply("A non-OK response (%s) was received as a result of an HTTP call",
                                            outcall.getResponse().statusCode()));
    }

    /**
     * Returns the response of the call as plain text.
     *
     * @return the response of the call as String
     * @throws IOException in case of an IO error while receiving the result
     * @deprecated use {@link #getInput()}}
     */
    @Deprecated
    public String getPlainInput() throws IOException {
        return outcall.getData();
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
