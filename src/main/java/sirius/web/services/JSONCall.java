/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.services;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import io.netty.handler.codec.http.HttpHeaderNames;
import sirius.kernel.xml.Outcall;
import sirius.web.http.MimeHelper;

import java.io.IOException;
import java.net.URI;
import java.nio.charset.StandardCharsets;

/**
 * Simple call to send JSON to a server (URL) and receive JSON back.
 */
public class JSONCall {

    private Outcall outcall;

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
     * Adds a custom header field to the call
     *
     * @param name  name of the field
     * @param value value of the field
     */
    public void addHeader(String name, String value) {
        outcall.setRequestProperty(name, value);
    }

    /**
     * Can be used to generate the JSON request.
     * <p>
     * This will mark the underlying {@link Outcall} as a POST request.
     *
     * @return the an input which can be used to generate a JSON document which is sent to the URL
     * @throws IOException in case of an IO error while sending the JSON document
     */
    public JSONStructuredOutput getOutput() throws IOException {
        return new JSONStructuredOutput(outcall.postFromOutput(), null, StandardCharsets.UTF_8.name());
    }

    /**
     * Provides access to the JSON answer of the call.
     *
     * @return the JSON result of the call
     * @throws IOException in case of an IO error while receiving the result
     */
    public JSONObject getInput() throws IOException {
        return JSON.parseObject(getPlainInput());
    }

    /**
     * Returns the response of the call as plain text.
     *
     * @return the response of the call as String
     * @throws IOException in case of an IO error while receiving the result
     */
    public String getPlainInput() throws IOException {
        return outcall.getData();
    }

    /**
     * Provides access to the underlying outcall.
     *
     * @return the underlying outcall
     */
    public Outcall getOutcall() {
        return outcall;
    }
}
