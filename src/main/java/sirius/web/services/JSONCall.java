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
import com.google.common.base.Charsets;
import com.google.common.io.ByteStreams;
import sirius.kernel.xml.Outcall;

import java.io.IOException;
import java.net.URL;

/**
 * Simple call to send JSON to a server (URL) and receive JSON back.
 *
 * @author Andreas Haufler (aha@scireum.de)
 * @since 2015/01
 */
public class JSONCall {

    private Outcall outcall;

    /**
     * Creates a new JSONCall for the given url with Content-Type 'application/json'.
     *
     * @param url the target URL to call
     * @return an <tt>JSONCall</tt> which can be used to send and receive JSON
     * @throws java.io.IOException in case of an IO error
     */
    public static JSONCall to(URL url) throws IOException {
        return to(url, "application/json; charset=" + Charsets.UTF_8.name());
    }

    /**
     * Creates a new JSONCall for the given url.
     *
     * @param url         the target URL to call
     * @param contentType the Content-Type to use
     * @return a new instance to perform the xml call
     * @throws IOException in case of an IO error
     */
    public static JSONCall to(URL url, String contentType) throws IOException {
        JSONCall result = new JSONCall();
        result.outcall = new Outcall(url);
        result.outcall.setRequestProperty("Content-Type", contentType);
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
     *
     * @return the an input which can be used to generate an XML document which is sent to the URL
     * @throws IOException in case of an IO error while sending the JSON document
     */
    public JSONStructuredOutput getOutput() throws IOException {
        return new JSONStructuredOutput(outcall.getOutput(), null, Charsets.UTF_8.name());
    }

    /**
     * Provides access to the JSON answer of the call.
     *
     * @return the JSON result of the call
     * @throws IOException in case of an IO error while receiving the result
     */
    @SuppressWarnings("unchecked")
    public JSONObject getInput() throws IOException {
        return JSON.parseObject(new String(ByteStreams.toByteArray(outcall.getInput()), outcall.getContentEncoding()));
    }
}
