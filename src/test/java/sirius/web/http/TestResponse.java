/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.http;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import io.netty.handler.codec.http.HttpHeaderNames;
import io.netty.handler.codec.http.HttpHeaders;
import io.netty.handler.codec.http.HttpResponseStatus;
import sirius.kernel.async.CallContext;
import sirius.kernel.async.Promise;
import sirius.kernel.commons.Streams;
import sirius.kernel.commons.Strings;
import sirius.kernel.commons.Value;
import sirius.kernel.health.Exceptions;
import sirius.kernel.xml.StructuredNode;
import sirius.kernel.xml.XMLStructuredInput;
import sirius.tagliatelle.Template;

import javax.annotation.Nullable;
import javax.xml.xpath.XPathExpressionException;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URI;
import java.net.URLConnection;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

/**
 * Created by {@link TestRequest#execute()} or {@link TestRequest#executeAsync()} and represents a response
 * generated for a test request.
 * <p>
 * Provides additional information like {@link #getStatus()} or {@link #getType()} to verify what kind of
 * response was created.
 */
public class TestResponse extends Response {

    protected TestResponse(TestRequest testRequest) {
        super(testRequest);
        responsePromise = testRequest.testResponsePromise;
        followRedirect = testRequest.followRedirect;
    }

    /**
     * Returns the call context which was active during the call.
     * <p>
     * Used e.g. by <tt>UserContextHelper</tt> to fetch the inner <tt>UserContext</tt> to check for
     * errors.
     *
     * @return the <tt>CallContext</tt> which was active while handling (responding to) the request.
     */
    public CallContext getCallContext() {
        return innerCallContext;
    }

    /**
     * Represents the types of result which can be captured.
     */
    public enum ResponseType {
        STATUS, TEMPORARY_REDIRECT, PERMANENT_REDIRECT, FILE, RESOURCE, ERROR, DIRECT, TEMPLATE, TUNNEL, STREAM
    }

    private ResponseType type;
    private HttpResponseStatus status;
    private String templateName;
    private List<Object> templateParameters = new ArrayList<>();
    private String redirectUrl;
    private File file;
    private byte[] content;
    private String errorMessage;
    private String tunnelTargetUrl;
    private Promise<TestResponse> responsePromise;
    private JSONObject jsonContent;
    private XMLStructuredInput xmlContent;
    private CallContext innerCallContext;
    private boolean followRedirect = false;

    /**
     * Returns the HTTP status set by the application.
     *
     * @return the http status of the response
     */
    public HttpResponseStatus getStatus() {
        return status;
    }

    /**
     * Returns the type of content created into the response
     *
     * @return the type of the response
     */
    public ResponseType getType() {
        return type;
    }

    /**
     * Contains the name of the Rythm template used to generate the response
     *
     * @return the name of the template used to generate the response
     */
    public String getTemplateName() {
        return templateName;
    }

    /**
     * Returns the template paramters passed to Rythm when rendering the template
     *
     * @return the template parameters used to render the response
     */
    public List<Object> getTemplateParameters() {
        return Collections.unmodifiableList(templateParameters);
    }

    /**
     * Returns the value of the parameter at the given zero based index.
     *
     * @param index the zero based index of the parameter to fetch
     * @return the value of the parameter or an empty value if fewer parameters where used
     */
    public Value getTemplateParameter(int index) {
        return Value.indexOf(index, templateParameters);
    }

    @Override
    public HttpHeaders headers() {
        return super.headers();
    }

    /**
     * Returns the target URL if the response is a redirect.
     *
     * @return the target url of the redirect
     */
    public String getRedirectUrl() {
        return redirectUrl;
    }

    /**
     * Returns the file which was sent as response for the request.
     *
     * @return the file when was sent as response
     */
    public File getFile() {
        return file;
    }

    /**
     * Returns the error message which was reported by the application. (Only valid for responses of type ERROR).
     *
     * @return the error message
     */
    public String getErrorMessage() {
        return errorMessage;
    }

    /**
     * Returns the target URL which would be tunnelled through if this response is a TUNNEL response.
     *
     * @return the target URL which is tunnelled through the server
     */
    public String getTunnelTargetUrl() {
        return tunnelTargetUrl;
    }

    public byte[] getRawContent() {
        return content;
    }

    public String getContentAsString() {
        return new String(content, StandardCharsets.UTF_8);
    }

    public JSONObject getContentAsJson() {
        if (jsonContent == null) {
            jsonContent = JSON.parseObject(getContentAsString());
        }
        return jsonContent;
    }

    public XMLStructuredInput getContentAsXML() {
        if (xmlContent == null) {
            try {
                xmlContent = new XMLStructuredInput(new ByteArrayInputStream(content), null);
            } catch (IOException e) {
                throw Exceptions.handle(e);
            }
        }
        return xmlContent;
    }

    public StructuredNode xmlContent() {
        try {
            return getContentAsXML().getNode(".");
        } catch (XPathExpressionException e) {
            throw Exceptions.handle(e);
        }
    }

    /**
     * Returns whether this response has a body that contains data
     *
     * @return whether this response has a body that contains data
     */
    public boolean isFilled() {
        switch (getType()) {
            case STATUS:
            case TEMPORARY_REDIRECT:
            case PERMANENT_REDIRECT:
            case ERROR:
                return true;
            case FILE:
                return getFile().length() > 0;
            case RESOURCE:
            case DIRECT:
            case TEMPLATE:
            case STREAM:
                return getRawContent().length > 0;
            case TUNNEL:
                try (InputStream stream = URI.create(getTunnelTargetUrl()).toURL().openStream()) {
                    return stream.available() > 0;
                } catch (IOException e) {
                    Exceptions.handle(e);
                    return true;
                }
        }
        return true;
    }

    /**
     * Returns whether this response has no body or contains no data
     *
     * @return whether this response has no body or contains no data
     */
    public boolean isEmpty() {
        return !isFilled();
    }

    @Override
    public void status(HttpResponseStatus status) {
        type = ResponseType.STATUS;
        this.status = status;
        completeResponse();
    }

    private void completeResponse() {
        innerCallContext = CallContext.getCurrent();
        responsePromise.success(this);
        wc.responseCompleted = true;

        if (wc.completionPromise != null) {
            wc.completionPromise.success(status.code());
        }
    }

    @Override
    public void redirectTemporarily(String url) {
        type = ResponseType.TEMPORARY_REDIRECT;
        status = HttpResponseStatus.TEMPORARY_REDIRECT;
        redirectUrl = url;
        completeResponse();
    }

    @Override
    public void redirectPermanently(String url) {
        type = ResponseType.PERMANENT_REDIRECT;
        status = HttpResponseStatus.MOVED_PERMANENTLY;
        redirectUrl = url;
        completeResponse();
    }

    @Override
    public void redirectToGet(String url) {
        if (followRedirect) {
            TestRequest redirectRequest = TestRequest.GET(url);
            redirectRequest.testResponsePromise = responsePromise;
            redirectRequest.execute();
            return;
        }
        type = ResponseType.TEMPORARY_REDIRECT;
        status = HttpResponseStatus.FOUND;
        redirectUrl = url;
        completeResponse();
    }

    @Override
    public void file(File file) {
        type = ResponseType.FILE;
        status = HttpResponseStatus.OK;
        this.file = file;
        completeResponse();
    }

    @Override
    public void resource(URLConnection urlConnection) {
        try {
            type = ResponseType.RESOURCE;
            status = HttpResponseStatus.OK;
            content = Streams.toByteArray(urlConnection.getInputStream());
            completeResponse();
        } catch (IOException e) {
            innerCallContext = CallContext.getCurrent();
            responsePromise.fail(e);
        }
    }

    @Override
    public void error(HttpResponseStatus status, String message) {
        type = ResponseType.ERROR;
        this.status = status;
        this.errorMessage = message;
        completeResponse();
    }

    @Override
    public void direct(HttpResponseStatus status, String content) {
        type = ResponseType.DIRECT;
        this.status = status;
        this.content = content.getBytes(StandardCharsets.UTF_8);
        completeResponse();
    }

    @Override
    public void template(HttpResponseStatus status, Template template, Object... params) {
        try {
            type = ResponseType.TEMPLATE;
            this.status = status;
            templateName = template.getName();
            templateParameters = Arrays.asList(params);
            super.template(status, template, params);
        } catch (Throwable e) {
            innerCallContext = CallContext.getCurrent();
            responsePromise.fail(e);
        }
    }

    @Override
    protected void sendTemplateContent(HttpResponseStatus status, String name, String content) {
        this.status = status;
        this.content = content.getBytes(StandardCharsets.UTF_8);
        completeResponse();
    }

    @Override
    public void tunnel(String url) {
        type = ResponseType.TUNNEL;
        status = HttpResponseStatus.OK;
        tunnelTargetUrl = url;
        completeResponse();
    }

    @Override
    public OutputStream outputStream(HttpResponseStatus status, @Nullable String contentType) {
        type = ResponseType.STREAM;
        this.status = status;
        if (Strings.isFilled(contentType)) {
            addHeaderIfNotExists(HttpHeaderNames.CONTENT_TYPE, contentType);
        }
        if (name != null) {
            setContentDisposition(name, download);
        }

        return new ByteArrayOutputStream() {
            @Override
            public void close() throws IOException {
                super.close();
                content = toByteArray();
                completeResponse();
            }
        };
    }
}
