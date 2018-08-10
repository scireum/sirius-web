/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.http;

import com.alibaba.fastjson.JSONObject;
import com.google.common.collect.Lists;
import io.netty.channel.ChannelHandlerContext;
import io.netty.handler.codec.DecoderResult;
import io.netty.handler.codec.http.DefaultHttpHeaders;
import io.netty.handler.codec.http.HttpHeaderNames;
import io.netty.handler.codec.http.HttpHeaders;
import io.netty.handler.codec.http.HttpMethod;
import io.netty.handler.codec.http.HttpRequest;
import io.netty.handler.codec.http.HttpVersion;
import io.netty.handler.codec.http.cookie.ClientCookieEncoder;
import io.netty.handler.codec.http.cookie.Cookie;
import io.netty.handler.codec.http.cookie.DefaultCookie;
import io.netty.handler.codec.http.multipart.Attribute;
import io.netty.handler.codec.http.multipart.MemoryAttribute;
import sirius.kernel.async.Barrier;
import sirius.kernel.async.CallContext;
import sirius.kernel.async.Promise;
import sirius.kernel.commons.Context;
import sirius.kernel.commons.Strings;
import sirius.kernel.commons.Value;
import sirius.kernel.di.std.Part;
import sirius.kernel.health.Exceptions;
import sirius.kernel.nls.NLS;
import sirius.web.resources.Resources;

import javax.annotation.Nonnull;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.time.Instant;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.StringJoiner;
import java.util.concurrent.TimeUnit;

/**
 * Provides a test or mock instance of {@link sirius.web.http.WebContext}.
 * <p>
 * Can be used to simulate a request sent to the web server. Additionally the {@link sirius.web.http.TestResponse}
 * returned by {@link #execute()} or {@link #executeAsync()} can be used to inspect what kind of response
 * was generated or which parameters were passed along.
 */
public class TestRequest extends WebContext implements HttpRequest {

    private static final String DUMMY_CSRF_TOKEN = "DUMMY-CSRF-TOKEN";

    @Part
    private static Resources resources;

    private HttpHeaders testHeaders = new DefaultHttpHeaders();
    private String testUri;
    private Map<String, Object> parameters = Context.create();
    private InputStream resource;
    private HttpMethod testMethod;
    private boolean preDispatch;
    private List<Cookie> testCookies = Lists.newArrayList();
    protected Promise<TestResponse> testResponsePromise = new Promise<>();
    private static DispatcherPipeline pipeline;
    private Map<String, String> testSession;

    protected TestRequest() {
        super();
        this.request = this;
        this.testSession = new HashMap<>();
    }

    public TestRequest(HttpMethod method, String uri) {
        this();
        this.testMethod = method;
        this.testUri = uri;
    }

    public static TestRequest GET(String uri) {
        return new TestRequest(HttpMethod.GET, uri);
    }

    public static TestRequest POST(String uri) {
        return new TestRequest(HttpMethod.POST, uri);
    }

    public static TestRequest DELETE(String uri) {
        return new TestRequest(HttpMethod.DELETE, uri);
    }

    public static TestRequest PUT(String uri) {
        return new TestRequest(HttpMethod.PUT, uri);
    }

    /**
     * Creates a mock request simulating a POST on the given uri.
     * <p>
     * The Request will be sent with a valid CSRF parameter so that it may pass a {@link WebContext#ensureSafePOST()} check.
     *
     * @param uri the relative uri to call
     * @return an instance used to further specify the request to send
     * @see CSRFHelper
     * @deprecated use {@link #POST(String)} and {@link #sendResource(String)}
     */
    public static TestRequest SAFEPOST(String uri) {
        TestRequest request = POST(uri);

        request.setTestSessionValue(CSRFHelper.CSRF_TOKEN, DUMMY_CSRF_TOKEN);
        request.setTestSessionValue(CSRFHelper.LAST_CSRF_RECOMPUTE, Value.of(Instant.now().toEpochMilli()).asString());
        request.withParameter(CSRFHelper.CSRF_TOKEN, DUMMY_CSRF_TOKEN);
        return request;
    }

    /**
     * Creates a mock request simulating a GET on the given uri and query string parameters
     *
     * @param uri         the relative uri to call
     * @param queryString the parameters to be applied to the query string of the request.
     *                    Commonly known as "GET parameters".
     * @return an instance used to further specify the request to send
     * @deprecated use {@link #GET(String)} and {@link #withParameters(Map)}
     */
    @Deprecated
    public static TestRequest GET(String uri, sirius.kernel.commons.Context queryString) {
        return GET(uri).withParameters(queryString);
    }

    /**
     * Creates a mock request simulating a PUT on the given uri while sending the given resource.
     * <p>
     * The resource will be resolved using {@link Resources}
     *
     * @param uri      the relative uri to call
     * @param resource the name of the resource to send
     * @return an instance used to further specify the request to send
     * @see #PUT(String, InputStream)
     * @deprecated use {@link #PUT(String)} and {@link #sendResource(String)}
     */
    @Deprecated
    public static TestRequest PUT(String uri, String resource) {
        return PUT(uri).sendResource(resource);
    }

    /**
     * Creates a mock request simulating a PUT on the given uri while sending the given data.
     *
     * @param uri      the relative uri to call
     * @param resource the data to send to the server
     * @return an instance used to further specify the request to send
     * @deprecated use {@link #PUT(String)} and {@link #sendResource(InputStream)}
     */
    @Deprecated
    public static TestRequest PUT(String uri, InputStream resource) {
        return PUT(uri).sendResource(resource);
    }

    /**
     * Creates a mock request simulating a POST on the given uri while sending the given resource.
     * <p>
     * The resource will be resolved using {@link Resources}
     *
     * @param uri      the relative uri to call
     * @param resource the name of the resource to send
     * @return an instance used to further specify the request to send
     * @see #POST(String, InputStream)
     * @deprecated use {@link #POST(String)} and {@link #sendResource(String)}
     */
    @Deprecated
    public static TestRequest POST(String uri, String resource) {
        return POST(uri).sendResource(resource);
    }

    /**
     * Creates a mock request simulating a POST on the given uri while sending the given data.
     *
     * @param uri      the relative uri to call
     * @param resource the data to send to the server
     * @return an instance used to further specify the request to send
     * @deprecated use {@link #POST(String)} and {@link #sendResource(InputStream)}
     */
    @Deprecated
    public static TestRequest POST(String uri, InputStream resource) {
        return POST(uri).sendResource(resource);
    }

    /**
     * Creates a mock request simulating a POST on the given uri and form data
     *
     * @param uri      the relative uri to call
     * @param formData the parameters be included in the post request
     * @return an instance used to further specify the request to send
     * @deprecated use {@link #POST(String)} and {@link #withParameters(Map)}
     */
    @Deprecated
    public static TestRequest POST(String uri, sirius.kernel.commons.Context formData) {
        return POST(uri).withParameters(formData);
    }

    /**
     * Creates a mock request simulating a POST on the given uri and JSON data
     *
     * @param uri  the relative uri to call
     * @param json the JSON data be included in the post request
     * @return an instance used to further specify the request to send
     * @deprecated use {@link #POST(String)} and {@link #sendData(JSONObject)}
     */
    @Deprecated
    public static TestRequest POST(String uri, JSONObject json) {
        return POST(uri).sendData(json);
    }

    /**
     * Adds a header included in the request.
     *
     * @param name  the name of the header to add.
     * @param value the value of the header to add
     * @return the request itself for fluent method calls
     */
    public TestRequest addHeader(CharSequence name, Object value) {
        testHeaders.add(name.toString(), value);
        return this;
    }

    /**
     * Adds a cookie included in the request.
     *
     * @param name  the name of the cookie to add.
     * @param value the value of the cookie to add
     * @return the request itself for fluent method calls
     */
    public TestRequest addCookie(String name, String value) {
        boolean updated = false;
        for (Cookie cookie : testCookies) {
            if (Strings.areEqual(name, cookie.name())) {
                cookie.setValue(value);
                updated = true;
            }
        }
        if (!updated) {
            testCookies.add(new DefaultCookie(name, value));
        }
        testHeaders.set(HttpHeaderNames.COOKIE, ClientCookieEncoder.STRICT.encode(testCookies));
        return this;
    }

    /**
     * Marks this request as preDispatchable, so that it will only match routes with
     * {@link sirius.web.controller.Routed#preDispatchable() preDispatchable}<tt> == true</tt>
     *
     * @return the request itself for fluent method calls
     */
    public TestRequest preDispatch() {
        this.preDispatch = true;
        return this;
    }

    public TestRequest withParameter(String key, Object value) {
        this.parameters.put(key, value);

        return this;
    }

    public TestRequest withParameters(Map<String, Object> parameters) {
        this.parameters.putAll(parameters);

        return this;
    }

    public TestRequest sendData(Map<String, Object> postData) {
        sendData(generateQueryString(postData));

        return this;
    }

    public TestRequest sendData(JSONObject postData) {
        sendData(postData.toString());

        return this;
    }

    public TestRequest sendData(String postData) {
        sendResource(new ByteArrayInputStream(postData.getBytes()));

        return this;
    }

    public TestRequest sendResource(String resource) {
        return sendResource(getResourceAsStream(resource));
    }

    public TestRequest sendResource(InputStream resource) {
        this.resource = resource;

        return this;
    }

    @Override
    public Response respondWith() {
        return new TestResponse(this);
    }

    @Override
    public ChannelHandlerContext getCtx() {
        return null;
    }

    @Override
    public boolean isValid() {
        return true;
    }

    @Override
    protected void setCtx(ChannelHandlerContext ctx) {
        throw new UnsupportedOperationException();
    }

    @Override
    @Deprecated
    public HttpMethod getMethod() {
        return testMethod;
    }

    @Override
    public HttpMethod method() {
        return testMethod;
    }

    @Override
    public HttpRequest setMethod(HttpMethod method) {
        throw new UnsupportedOperationException();
    }

    @Override
    public String uri() {
        return testUri;
    }

    @Override
    @Deprecated
    public String getUri() {
        return testUri;
    }

    @Override
    public HttpRequest setUri(String uri) {
        throw new UnsupportedOperationException();
    }

    @Override
    @Deprecated
    public HttpVersion getProtocolVersion() {
        return HttpVersion.HTTP_1_1;
    }

    @Override
    public HttpVersion protocolVersion() {
        return HttpVersion.HTTP_1_1;
    }

    @Override
    public HttpRequest setProtocolVersion(HttpVersion version) {
        throw new UnsupportedOperationException();
    }

    @Override
    public Value getSessionValue(String key) {
        return super.getSessionValue(key).isFilled() ? super.getSessionValue(key) : Value.of(testSession.get(key));
    }

    @Override
    public HttpHeaders headers() {
        return testHeaders;
    }

    @Deprecated
    @Override
    public DecoderResult getDecoderResult() {
        return DecoderResult.SUCCESS;
    }

    @Override
    public DecoderResult decoderResult() {
        return DecoderResult.SUCCESS;
    }

    @Override
    public void setDecoderResult(DecoderResult result) {
        throw new UnsupportedOperationException();
    }

    @Override
    public String toString() {
        return "TestRequest: " + uri();
    }

    /**
     * Executes the request (dispatches it).
     * <p>
     * Returns a promise which will be full filled, once the called code produces a response. Use
     * {@link #execute()} to wait until a response is generated.
     *
     * @return a promise which will contain the response generated by the application
     */
    public Promise<TestResponse> executeAsync() {
        build();

        CallContext.getCurrent().set(WebContext.class, this);
        try {
            if (preDispatch && getPipeline().preDispatch(this)) {
                contentHandler.handle(this.content.getByteBuf(), true);
            } else {
                getPipeline().dispatch(this);
            }
            return testResponsePromise;
        } catch (Exception e) {
            throw Exceptions.handle(e);
        }
    }

    protected void build() {
        // append GET parameters to URI
        String queryString = generateQueryString(parameters);
        this.testUri =
                this.testUri + (Strings.isFilled(queryString) ? (testUri.contains("?") ? "&" : "?") + queryString : "");

        // send resource in body
        if (resource != null && (testMethod.equals(HttpMethod.PATCH) || testMethod.equals(HttpMethod.POST) || testMethod
                .equals(HttpMethod.PUT))) {
            try {
                addHeader(HttpHeaderNames.CONTENT_LENGTH, resource.available());
                Attribute body = new MemoryAttribute("body");
                body.setContent(resource);
                content = body;
            } catch (IOException e1) {
                throw Exceptions.handle(e1);
            }
        }
    }

    /**
     * Executes (dispatches) the request and waits until a response is generated.
     *
     * @return the generated response
     * @deprecated use {@link #execute()}
     */
    @Deprecated
    public TestResponse executeAndBlock() {
        return execute();
    }

    /**
     * Executes (dispatches) the request and waits until a response is generated.
     *
     * @return the generated response
     */
    public TestResponse execute() {
        Barrier barrier = Barrier.create();
        barrier.add(executeAsync());
        if (barrier.await(60, TimeUnit.SECONDS)) {
            if (testResponsePromise.isSuccessful()) {
                return testResponsePromise.get();
            } else {
                throw Exceptions.handle()
                                .withSystemErrorMessage("Failed to create a response for: %s", uri())
                                .error(testResponsePromise.getFailure())
                                .handle();
            }
        } else {
            throw Exceptions.handle().withSystemErrorMessage("No response was created after 60s: %s", uri()).handle();
        }
    }

    @Nonnull
    protected String generateQueryString(Map<String, Object> parameters) {
        if (parameters.isEmpty()) {
            return "";
        }

        StringJoiner joiner = new StringJoiner("&");
        for (Map.Entry<String, Object> param : parameters.entrySet()) {
            if (param.getValue() instanceof Iterable) {
                for (Object paramExpandedValue : (Iterable<?>) param.getValue()) {
                    joiner.add(generateQueryStringParam(param.getKey(), paramExpandedValue));
                }
            } else {
                joiner.add(generateQueryStringParam(param.getKey(), param.getValue()));
            }
        }
        return joiner.toString();
    }

    private String generateQueryStringParam(String key, Object value) {
        return Strings.urlEncode(key) + "=" + Strings.urlEncode(NLS.toMachineString(value));
    }

    protected InputStream getResourceAsStream(String resource) {
        return resources.resolve(resource)
                        .orElseThrow(() -> new IllegalArgumentException("Unknown Resource: " + resource))
                        .openStream();
    }

    private DispatcherPipeline getPipeline() {
        if (pipeline == null) {
            pipeline = DispatcherPipeline.create();
        }

        return pipeline;
    }

    private void setTestSessionValue(String key, Object value) {
        if (value == null) {
            testSession.remove(key);
        } else {
            testSession.put(key, NLS.toMachineString(value));
        }
    }
}
