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
import io.netty.buffer.ByteBuf;
import io.netty.channel.ChannelHandlerContext;
import io.netty.handler.codec.http.HttpHeaderNames;
import io.netty.handler.codec.http.HttpMethod;
import io.netty.handler.codec.http.HttpRequest;
import io.netty.handler.codec.http.HttpResponseStatus;
import io.netty.handler.codec.http.QueryStringDecoder;
import io.netty.handler.codec.http.QueryStringEncoder;
import io.netty.handler.codec.http.cookie.Cookie;
import io.netty.handler.codec.http.cookie.CookieHeaderNames;
import io.netty.handler.codec.http.cookie.DefaultCookie;
import io.netty.handler.codec.http.cookie.ServerCookieDecoder;
import io.netty.handler.codec.http.multipart.Attribute;
import io.netty.handler.codec.http.multipart.FileUpload;
import io.netty.handler.codec.http.multipart.HttpData;
import io.netty.handler.codec.http.multipart.HttpPostRequestDecoder;
import io.netty.handler.codec.http.multipart.InterfaceHttpData;
import io.netty.handler.codec.http.multipart.InterfaceHttpPostRequestDecoder;
import io.netty.util.IllegalReferenceCountException;
import sirius.kernel.Sirius;
import sirius.kernel.async.CallContext;
import sirius.kernel.async.Promise;
import sirius.kernel.async.SubContext;
import sirius.kernel.commons.Callback;
import sirius.kernel.commons.Explain;
import sirius.kernel.commons.Files;
import sirius.kernel.commons.Hasher;
import sirius.kernel.commons.Streams;
import sirius.kernel.commons.Strings;
import sirius.kernel.commons.Tuple;
import sirius.kernel.commons.Value;
import sirius.kernel.commons.Values;
import sirius.kernel.di.std.ConfigValue;
import sirius.kernel.di.std.Part;
import sirius.kernel.health.Exceptions;
import sirius.kernel.health.HandledException;
import sirius.kernel.health.Log;
import sirius.kernel.nls.NLS;
import sirius.kernel.xml.BasicNamespaceContext;
import sirius.kernel.xml.StructuredInput;
import sirius.kernel.xml.XMLStructuredInput;

import javax.annotation.CheckReturnValue;
import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import javax.xml.namespace.NamespaceContext;
import javax.xml.parsers.DocumentBuilderFactory;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.InetAddress;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.charset.UnsupportedCharsetException;
import java.time.Duration;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Base64;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.TreeMap;
import java.util.UUID;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

/**
 * Provides access to a request received by the WebServer.
 * <p>
 * This can be used to obtain all infos received for a HTTP request and also to create an appropriate response.
 * <p>
 * This context can either be passed along as variable or be accessed using {@link CallContext#get(Class)}
 */
public class WebContext implements SubContext {

    private static final String HEADER_X_FORWARDED_PROTO = "X-Forwarded-Proto";
    private static final String PROTOCOL_HTTPS = "https";
    private static final String PROTOCOL_HTTP = "http";
    private static final String SESSION_PIN_KEY = "_pin";
    private static final Log SESSION_CHECK = Log.get("session-check");
    private static final long SESSION_PIN_COOKIE_TTL = TimeUnit.DAYS.toSeconds(10L * 365);

    /**
     * How the secure property of a cookie should be set
     */
    public enum CookieSecurity {
        /**
         * the cookie shall only be sent via https
         */
        ALWAYS_SECURE,
        /**
         * no special security handling is required
         */
        NEVER,
        /**
         * like {@link #ALWAYS_SECURE} if the cookie is set via an SSL connection, otherwise like {@link #NEVER}
         */
        IF_SSL,
    }

    /**
     * Underlying channel to send and receive data
     */
    private ChannelHandlerContext ctx;

    /**
     * Internal attributes which can be set and read back during processing. This will not contain any posted or
     * other parameters.
     */
    private Map<String, Object> attribute;

    /**
     * The underlying request created by netty
     */
    protected HttpRequest request;

    /**
     * The effective request uri (without the query string)
     */
    private String requestedURI;

    /**
     * The effective request uri (without the query string)
     */
    private String rawRequestedURI;

    /**
     * The base url (without the uri, like: http://myhost.com)
     */
    private String baseURL;

    /**
     * Contains the parameters submitted in the query string (?param=value...)
     */
    private Map<String, List<String>> queryString;

    /**
     * Contains decoded cookies which where sent by the client
     */
    private Map<String, Cookie> cookiesIn;

    /**
     * Contains cookies which will be sent to the client
     */
    protected Map<String, Cookie> cookiesOut;

    /**
     * Stores the decoder which was used to process a POST or PUT request
     */
    protected InterfaceHttpPostRequestDecoder postDecoder;

    /**
     * Sometimes it is useful to "hide" the fact that this is a POST request.
     * One case are login-forms. There are submitted for any URL but must not
     * interact with other POST handlers. Therefore, a user manager can
     * call hidePost() so that isPOST() will return false even if a post request
     * is present.
     */
    protected boolean hidePost = false;

    /**
     * A list of files to deleted once this call is handled
     */
    private List<File> filesToCleanup;

    /**
     * If the submitted data (from the client) was stored to a file, this will be stored here
     */
    private File contentAsFile;

    /**
     * Raw content submitted via POST or PUT
     */
    protected Attribute content;

    /**
     * Contains decoded data of the client session - this is sent back and forth using a cookie. This data
     * will not be stored on the server.
     */
    private Map<String, String> session;

    /**
     * Internal key used to keep track of the TTL of the client session cookie
     */
    private static final String TTL_SESSION_KEY = "_TTL";

    /**
     * Stores the effective session cookie TTL. If null "defaultSessionCookieTTL" is used.
     */
    private Long sessionCookieTTL;

    /**
     * Determines if the client session was modified and should be re-set via a cookie
     */
    private volatile boolean sessionModified;

    /**
     * Specifies the micro-timing key used for this request. If null, no micro-timing will be recorded.
     */
    protected String microtimingKey;

    /**
     * Used by Response - but stored here, since a new Response might be created....
     */
    protected volatile boolean responseCommitted;

    /**
     * Used by Response - but stored here, since a new Response might be created....
     */
    protected volatile boolean responseCompleted;

    /**
     * Invoked once the call is completely handled
     */
    protected Callback<CallContext> completionCallback;

    /**
     * This promise is completed once the request is completely handled.
     * <p>
     * The main purpose is to attach a callback once the request was successfully handled.
     */
    protected Promise<Integer> completionPromise;

    /**
     * Determines if the request is performed via a secured channel (SSL)
     */
    protected Boolean ssl;

    /**
     * Contains the remote IP. If a proxyIP is specified (WebServer#proxyIPs), an X-Forwarded-For header is checked
     */
    private InetAddress remoteIp;

    /**
     * If longCall is set to true (by the user), the idle-state handler is disabled for this request.
     */
    private volatile boolean longCall;

    /**
     * If set, will be supplied with all incoming content (instead of buffering on disk or in memory)
     */
    protected ContentHandler contentHandler;

    /**
     * Contains the timestamp this request was dispatched. (Will not be filled in pre-dispatch, as we only
     * want to measure how long it takes to generate an "average" result, not how long an upload took....)
     */
    protected volatile long started = 0;

    /**
     * Contains the timestamp this request was scheduled for execution.
     * This can be used to measure the actual execution time without the wait time if the thread pool
     * is fully utilized and requests are queued.
     */
    protected volatile long scheduled = 0;

    /**
     * Contains the timestamp this request was committed (a response was created).
     * This can be used to actually measure the server performance and not the download speed of clients.
     */
    protected volatile long committed = 0;

    /**
     * Caches the content size as the "readableBytes" value changes once a stream is on it.
     */
    private Long contentSize;

    /**
     * Caches the user agent for this request.
     */
    protected UserAgent userAgent;

    /**
     * Name of the cookie used to store and load the client session
     */
    @ConfigValue("http.sessionCookie.name")
    private static String sessionCookieName;

    /**
     * Determines if a session should be pinned to a certain client or user-agent by using another cookie.
     */
    @ConfigValue("http.sessionCookie.pinSession")
    private static boolean sessionPinning;

    /**
     * Name of the session pin cookie used to validate a corresponding client session cookie
     */
    @ConfigValue("http.sessionCookie.pinName")
    private static String sessionPinCookieName;

    /**
     * Name of previously used session pin cookie names that are still read for validation
     */
    @ConfigValue("http.sessionCookie.pinLegacyNames")
    private static List<String> sessionPinLegacyCookieNames;

    /**
     * The ttl of the client session cookie. If this is 0, it will be a "session cookie" and therefore
     * be deleted when the browser is closed
     */
    @ConfigValue("http.sessionCookie.ttl")
    private static Duration defaultSessionCookieTTL;

    /**
     * The same site attribute of the Session Cookie
     */
    @ConfigValue("http.sessionCookie.sameSite")
    private static CookieHeaderNames.SameSite sessionCookieSameSite;

    /**
     * The same site attribute of the Session Cookie
     */
    @ConfigValue("http.sessionCookie.secure")
    private static CookieSecurity sessionCookieSecurity;

    /**
     * Determines the domain set for all cookies. If empty no domain will be set.
     * If a cookie's domain attribute is not set, the cookie is only applicable to the domain of the originating request, EXCLUDING all its subdomains.
     * (However in IE 9 and older versions, a cookie made for abc.com is also sent in requests to xyz.abc.com)
     * If a cookie's domain attribute is set, the cookie is applicable to that domain, INCLUDING all its subdomains.
     * This value must be the same as or a parent of the domain of the originating request.
     * This value should not have a leading dot.
     */
    @ConfigValue("http.cookieDomain")
    @Nullable
    private static String cookieDomain;

    /**
     * Shared secret used to protect the client session. If empty one will be created on startup.
     */
    @ConfigValue("http.sessionSecret")
    private static String sessionSecret;

    /**
     * Input size limit for structured data (as this is loaded into heap)
     */
    @ConfigValue("http.maxStructuredInputSize")
    private static long maxStructuredInputSize;

    /**
     * Determines if a dummy P3P header should be created to disable P3P handling.
     */
    @ConfigValue("http.addP3PHeader")
    protected static boolean addP3PHeader;

    /**
     * Determines the security policy used by the client when loading internet resources.
     */
    @ConfigValue("http.contentSecurityPolicy")
    protected static String contentSecurityPolicy;

    /**
     * Should the automatic CORS handling be done or not?
     */
    @ConfigValue("http.corsAllowAll")
    protected static boolean corsAllowAll;

    /**
     * Should a Strict-Transport-Security header be sent?
     */
    @ConfigValue("http.ssl.forceHSTS")
    protected static boolean forceHSTS;

    /**
     * Should the automatic CORS handling be done or not?
     */
    @ConfigValue("http.ssl.hstsMaxAge")
    protected static int hstsMaxAge;

    @Part
    @Nullable
    private static SessionSecretComputer sessionSecretComputer;

    @Part
    private static CSRFHelper csrfHelper;

    /**
     * Provides access to the underlying ChannelHandlerContext
     *
     * @return the underlying channel handler context
     */
    public ChannelHandlerContext getCtx() {
        return ctx;
    }

    /**
     * Enables micro-timing for this request.
     * <p>
     * If <tt>null</tt> is passed in as key, the request uri is used.
     * <p>
     * If the micro-timing was already enabled, it will remain enabled, with the original key
     *
     * @param key the key used to pass to the micro-timing framework.
     * @return <tt>this</tt> to fluently work with this context
     */
    public WebContext enableTiming(String key) {
        if (microtimingKey == null) {
            if (key == null) {
                microtimingKey = getRequestedURI();
            } else {
                microtimingKey = key;
            }
        }

        return this;
    }

    /**
     * Determines if this context is attached to a request or not.
     *
     * @return <tt>true</tt> if this context is attached to a request, <tt>false</tt> otherwise
     */
    public boolean isValid() {
        return request != null;
    }

    /**
     * Used to provide a handle which is invoked once the call is completely handled.
     * <p>
     * Note that calling this method, removes the last completion handler. Also, this runs
     * in a very central IO thread pool, therefore this should only be used in technical
     * scenarios.
     *
     * @param onComplete the handler to be invoked once the request is completely handled
     */
    public void onComplete(Callback<CallContext> onComplete) {
        completionCallback = onComplete;
    }

    /**
     * Provides a promise which is fulfilled with the HTTP status code once the request has been completely
     * handled.
     *
     * @return a promise to attach handlers which are invoked once the request was successfully handled
     */
    public Promise<Integer> getCompletionPromise() {
        if (completionPromise == null) {
            if (responseCompleted) {
                return new Promise<Integer>().success(HttpResponseStatus.OK.code());
            }

            completionPromise = new Promise<>();
        }

        return completionPromise;
    }

    /**
     * Sets the ChannelHandlerContext for this context.
     *
     * @param ctx the channel handler context to use
     */
    protected void setCtx(ChannelHandlerContext ctx) {
        this.ctx = ctx;
    }

    /**
     * Provides access to the underlying netty HttpRequest
     *
     * @return the underlying request
     */
    public HttpRequest getRequest() {
        return request;
    }

    /**
     * Sets the underlying HttpRequest
     *
     * @param request the request on which this context is based
     */
    protected void setRequest(HttpRequest request) {
        this.request = request;
    }

    /**
     * Determines if this request was marked as long call.
     * <p>
     * This will effectively disable the idle timeout for this request.
     *
     * @return <tt>true</tt> if the request was marked as long call, <tt>false</tt> otherwise
     */
    public boolean isLongCall() {
        return longCall;
    }

    /**
     * Marks the request as long call.
     * <p>
     * This will disable all idle timeout checks for this request.
     */
    public void markAsLongCall() {
        this.longCall = true;
    }

    /**
     * Can be set from within {@link WebDispatcher#preparePreDispatch(WebContext)} to manually handle incoming content.
     *
     * @param handler the handler to be supplied with content. If <tt>null</tt>, the default (memory/disk buffering)
     *                handler is applied.
     */
    public void setContentHandler(ContentHandler handler) {
        this.contentHandler = handler;
    }

    /**
     * Returns a value or parameter supplied by the request.
     * <p>
     * This method first checks if an attribute with the given key exists. If not, the query string is scanned. After
     * that, the posted content is looked through to find an appropriate value.
     *
     * @param key the key used to look for the value
     * @return a Value representing the provided data.
     */
    @Nonnull
    public Value get(String key) {
        if (attribute != null && attribute.containsKey(key)) {
            return Value.of(attribute.get(key));
        }

        if (queryString == null) {
            decodeQueryString();
        }

        if (queryString.containsKey(key)) {
            List<String> val = getParameters(key);
            if (val.size() == 1) {
                return Value.of(val.get(0));
            } else if (val.isEmpty()) {
                return Value.EMPTY;
            } else {
                return Value.of(val);
            }
        }

        if (postDecoder != null) {
            return fetchPostAttributes(key);
        }

        return Value.EMPTY;
    }

    @SuppressWarnings("java:S6204")
    @Explain("We return a mutable list here, as we cannot predict what the caller does with the result.")
    private Value fetchPostAttributes(String key) {
        try {
            List<InterfaceHttpData> dataList = postDecoder.getBodyHttpDatas(key);
            if (dataList == null || dataList.isEmpty()) {
                return Value.EMPTY;
            } else if (dataList.size() == 1) {
                return Value.of(transformHttpData(dataList.get(0)));
            } else {
                List<String> attributes = dataList.stream()
                                                  .map(this::transformHttpData)
                                                  .filter(Strings::isFilled)
                                                  .collect(Collectors.toList());
                if (!attributes.isEmpty()) {
                    return Value.of(attributes);
                }
            }
        } catch (HttpPostRequestDecoder.NotEnoughDataDecoderException e) {
            Exceptions.ignore(e);
        } catch (Exception e) {
            Exceptions.handle()
                      .to(WebServer.LOG)
                      .error(e)
                      .withSystemErrorMessage("Failed to fetch parameter %s: %s (%s)", key)
                      .handle();
        }
        return Value.EMPTY;
    }

    private String transformHttpData(InterfaceHttpData data) {
        try {
            if (data instanceof Attribute attr) {
                ByteBuf byteBuf = attr.getByteBuf();

                // If the request gets aborted prematurely, the underlying buffers might
                // already be released. Therefore, we have to check this here manually as
                // the server might still try to process the request...
                if (byteBuf != null) {
                    return byteBuf.toString(attr.getCharset());
                }
            }
        } catch (IOException | IllegalReferenceCountException e) {
            Exceptions.ignore(e);
        }
        return null;
    }

    /**
     * Returns a {@link File file} supplied by the request as POST-parameter.
     *
     * @param key used to specify which part of the post request should be used.
     * @return a {@link File file} sent for the given key or <tt>null</tt> if none is available
     * @throws IOException in case of an IO error
     */
    @Nullable
    public File getFile(String key) throws IOException {
        FileUpload fileUpload = getFileData(key);
        if (fileUpload == null) {
            return null;
        }
        if (!fileUpload.isInMemory()) {
            return fileUpload.getFile();
        }
        File temp = File.createTempFile("http", "");
        addFileToCleanup(temp);
        try (FileOutputStream outputStream = new FileOutputStream(temp)) {
            outputStream.write(fileUpload.get());
        }
        return temp;
    }

    /**
     * Returns the first non-empty value for the given keys.
     * <p>
     * This is a boilerplate method for {@link #get(String)} in case the same value could be sent via different
     * parameter names.
     *
     * @param keys the keys to check
     * @return the first non-empty value or an empty value if no data was found for all given keys.
     */
    public Value getFirstFilled(String... keys) {
        for (String key : keys) {
            Value result = get(key);
            if (result.isFilled()) {
                return result;
            }
        }
        return Value.EMPTY;
    }

    /**
     * Determines if the parameter with the given name is contained in the request. Either as POST value or in the
     * query string.
     *
     * @param key the parameter to check for
     * @return <tt>true</tt> if the parameter is present (even if its value is <tt>null</tt>), <tt>false</tt> otherwise
     */
    public boolean hasParameter(String key) {
        if (attribute != null && attribute.containsKey(key)) {
            return true;
        }
        if (postDecoder != null) {
            try {
                InterfaceHttpData data = postDecoder.getBodyHttpData(key);
                if (data instanceof Attribute) {
                    return true;
                }
            } catch (Exception e) {
                Exceptions.handle(WebServer.LOG, e);
            }
        }
        if (queryString == null) {
            decodeQueryString();
        }
        return queryString.containsKey(key);
    }

    /**
     * Returns the value provided for the given key(s) or reports an error if no non-empty value was found.
     * <p>
     * The first non-empty value is used. If all values are empty, an exception is thrown.
     *
     * @param keys the keys to check for a value
     * @return the first non-empty value found for one of the given keys
     */
    public Value require(String... keys) {
        for (String key : keys) {
            Value result = get(key);
            if (result.isFilled()) {
                return result;
            }
        }
        throw Exceptions.createHandled()
                        .withSystemErrorMessage(
                                "A required parameter was not filled. Provide at least one value for: %s",
                                Arrays.asList(keys))
                        .handle();
    }

    /**
     * Returns the posted part with the given key.
     *
     * @param key used to specify which part of the post request should be returned.
     * @return the data provided for the given key or <tt>null</tt> if no data was supplied.
     */
    public HttpData getHttpData(String key) {
        if (postDecoder == null) {
            return null;
        }
        try {
            InterfaceHttpData data = postDecoder.getBodyHttpData(key);
            if (data instanceof HttpData httpData) {
                return httpData;
            }
        } catch (Exception e) {
            Exceptions.handle(WebServer.LOG, e);
        }
        return null;
    }

    /**
     * Returns the file upload supplied for the given key.
     *
     * @param key used to specify which part of the post request should be used.
     * @return a file upload sent for the given key or <tt>null</tt> if no upload data is available
     */
    public FileUpload getFileData(String key) {
        if (postDecoder == null) {
            return null;
        }
        try {
            InterfaceHttpData data = postDecoder.getBodyHttpData(key);
            if (data instanceof FileUpload fileUpload) {
                return fileUpload;
            }
        } catch (Exception e) {
            Exceptions.handle(WebServer.LOG, e);
        }
        return null;
    }

    /**
     * Sets an attribute for the current request.
     * <p>
     * Attributes are neither stored nor transmitted to the client. Therefore, they are only visible during the
     * processing of this request.
     *
     * @param key   name of the attribute
     * @param value value of the attribute
     */
    public void setAttribute(String key, Object value) {
        if (attribute == null) {
            attribute = new TreeMap<>();
        }
        attribute.put(key, value);
    }

    /*
     * Loads and parses the client session (cookie)
     */
    private void initSession() {
        String encodedSession = getCookieValue(sessionCookieName);
        if (Strings.isFilled(encodedSession)) {
            session = decodeSession(encodedSession);
            checkAndEnforceSessionPinning();
        } else {
            session = new HashMap<>();
        }
    }

    /**
     * Checks if the session pin matches the one stored in the session (if session pinning is active).
     * <p>
     * The idea of session pinning is to use a separate cookie which is updated way less frequently
     * (theoretically once every 10 years) and to store this value in the session itself.
     * <p>
     * Not if (due to a software bug or bad behaviour) someone manages to obtain the session itself
     * (i.e. a request containing the set-cookie header for SIRIUS_SESSION is cached or logged), the
     * session will still not be accepted, as the outside cookie and its value is missing.
     * <p>
     * The system will ensure that both values are never updated or set in the same request.
     */
    private void checkAndEnforceSessionPinning() {
        String givenSessionPin = getCookieValue(getSessionPinCookieName());
        String sessionPin = session.get(SESSION_PIN_KEY);
        String effectiveSessionPin = calculateEffectiveSessionPin(givenSessionPin);

        if (Strings.isFilled(sessionPin) && !Strings.areEqual(sessionPin, effectiveSessionPin) && !isLegacyCookieValid(
                sessionPin)) {
            SESSION_CHECK.SEVERE(Strings.apply("Session pin mismatch: %s (%s) vs. %s%n%s%n%s%nIP: %s",
                                               givenSessionPin,
                                               effectiveSessionPin,
                                               sessionPin,
                                               session,
                                               this,
                                               getRemoteIP()));
            clearSession();
        } else if (Strings.isEmpty(sessionPin) && Strings.isFilled(givenSessionPin)) {
            if (SESSION_CHECK.isFINE()) {
                SESSION_CHECK.FINE("Attaching session pin %s (%s) to %s%n%s%nIP: %s",
                                   givenSessionPin,
                                   effectiveSessionPin,
                                   session,
                                   this,
                                   getRemoteIP());
            }
            setSessionValue(SESSION_PIN_KEY, effectiveSessionPin);
        }
    }

    private boolean isLegacyCookieValid(String sessionPin) {
        return sessionPinLegacyCookieNames.stream()
                                          .map(this::getCookieValue)
                                          .filter(Objects::nonNull)
                                          .map(this::calculateEffectiveSessionPin)
                                          .anyMatch(effectiveSessionPin -> Strings.areEqual(effectiveSessionPin,
                                                                                            sessionPin));
    }

    private String calculateEffectiveSessionPin(String givenSessionPin) {
        return Strings.isEmpty(givenSessionPin) ? "" : Hasher.md5().hash(givenSessionPin).toHexString();
    }

    private String getSessionPinCookieName() {
        return sessionPinCookieName;
    }

    /**
     * Creates and installs a "session pin" cookie.
     * <p>
     * This is only done, if the framework is enabled, a session exists and the session was not modified in this request.
     */
    private void installSessionPinningCookieIfRequired() {
        if (!sessionPinning) {
            return;
        }

        String givenSessionPin = getCookieValue(getSessionPinCookieName());
        if (Strings.isFilled(givenSessionPin)) {
            return;
        }

        // Generate a pin which is stable for one day, as some requests might concurrently want to set the
        // session pin.
        givenSessionPin = Hasher.md5()
                                .hash(getRemoteIP().toString()
                                      + getHeader(HttpHeaderNames.USER_AGENT)
                                      + TimeUnit.MILLISECONDS.toMinutes(System.currentTimeMillis()))
                                .toHexString();

        if (SESSION_CHECK.isFINE()) {
            SESSION_CHECK.FINE("Creating session pin %s for %s%n%s%nIP: %s",
                               givenSessionPin,
                               session,
                               this,
                               getRemoteIP());
        }

        setCookie(getSessionPinCookieName(),
                  givenSessionPin,
                  SESSION_PIN_COOKIE_TTL,
                  sessionCookieSameSite,
                  sessionCookieSecurity);
    }

    private Map<String, String> decodeSession(String encodedSession) {
        Tuple<String, String> sessionInfo = Strings.split(encodedSession, ":");
        Map<String, String> decodedSession = new HashMap<>();
        long decodedSessionTTL = -1;
        QueryStringDecoder qsd = new QueryStringDecoder(encodedSession);
        for (Map.Entry<String, List<String>> entry : qsd.parameters().entrySet()) {
            if (TTL_SESSION_KEY.equals(entry.getKey())) {
                decodedSessionTTL = Values.of(entry.getValue()).at(0).getLong();
            } else {
                decodedSession.put(entry.getKey(), Values.of(entry.getValue()).at(0).getString());
            }
        }
        if (checkSessionDataIntegrity(decodedSession, sessionInfo)) {
            if (decodedSessionTTL >= 0) {
                sessionCookieTTL = decodedSessionTTL;
            }
            return decodedSession;
        } else {
            if (SESSION_CHECK.isFINE()) {
                SESSION_CHECK.FINE("Resetting client session due to inconsistent security hash: %s%n%s%nURI: %s%nIP: %s",
                                   encodedSession,
                                   this,
                                   getRequestedURL(),
                                   getRemoteIP());
            }
            return new HashMap<>();
        }
    }

    private boolean checkSessionDataIntegrity(Map<String, String> currentSession, Tuple<String, String> sessionInfo) {
        return Strings.areEqual(sessionInfo.getFirst(),
                                Hasher.sha512()
                                      .hash(sessionInfo.getSecond() + getSessionSecret(currentSession))
                                      .toHexString());
    }

    /**
     * Sets an explicit session cookie TTL (time to live).
     * <p>
     * If a non-null value is given, this will overwrite {@link #defaultSessionCookieTTL} for this request/response.
     *
     * @param customSessionCookieTTL the new TTL for the client session cookie.
     */
    public void setCustomSessionCookieTTL(@Nullable Duration customSessionCookieTTL) {
        this.sessionCookieTTL = customSessionCookieTTL == null ? null : customSessionCookieTTL.getSeconds();
    }

    /**
     * Stores a value in the client session.
     * <p>
     * As this session is transmitted to the client, the given value should not be large and needs a parseable
     * string representation
     *
     * @param key   the name of th value to set
     * @param value the value to set
     */
    public void setSessionValue(String key, Object value) {
        if (session == null) {
            initSession();
        }

        if (value == null) {
            String previous = session.remove(key);

            if (Strings.isFilled(previous)) {
                sessionModified = true;
            }
        } else {
            String newValue = NLS.toMachineString(value);
            String previous = session.put(key, newValue);

            if (!Objects.equals(previous, newValue)) {
                sessionModified = true;
            }
        }
    }

    /**
     * Loads a value from the client session
     *
     * @param key the name of the value to load
     * @return the value previously set in the session or an empty Value if no data is present
     */
    public Value getSessionValue(String key) {
        if (session == null) {
            initSession();
        }
        return Value.of(session.get(key));
    }

    /**
     * Returns a list of all known session keys for the current session
     *
     * @return a list of all known keys for the current session
     */
    public List<String> getSessionKeys() {
        if (session == null) {
            initSession();
        }
        return new ArrayList<>(session.keySet());
    }

    /**
     * Clears (invalidated) the client session by removing all values.
     */
    public void clearSession() {
        if (session == null) {
            session = new HashMap<>();
        }

        session.clear();
        sessionModified = true;
    }

    /**
     * Returns the decoded requested URI of the underlying HTTP request, without the query string
     *
     * @return the decoded uri of the underlying request
     */
    public String getRequestedURI() {
        if (requestedURI == null && request != null) {
            decodeQueryString();
        }
        return requestedURI;
    }

    /**
     * Returns the raw un-decoded requested URI of the underlying HTTP request, without the query string
     *
     * @return the un-decoded uri of the underlying request
     */
    public String getRawRequestedURI() {
        if (rawRequestedURI == null && request != null) {
            rawRequestedURI = stripQueryFromURI(request.uri());
        }
        return rawRequestedURI;
    }

    /**
     * Returns the base url (the protocol + host) for which this request was made.
     *
     * @return the base url which created this request, without the actual URI
     */
    public String getBaseURL() {
        if (baseURL == null) {
            StringBuilder sb = new StringBuilder();
            sb.append(isSSL() ? PROTOCOL_HTTPS : PROTOCOL_HTTP);
            sb.append("://");
            if (getRequest().headers().contains("X-Forwarded-Host")) {
                sb.append(getHeader("X-Forwarded-Host"));
            } else {
                sb.append(getHeader(HttpHeaderNames.HOST));
            }
            baseURL = sb.toString();
        }

        return baseURL;
    }

    /**
     * Returns the complete URL as requested by the browser.
     *
     * @return the complete url (base url + uri) which created this request
     */
    public String getRequestedURL() {
        return getBaseURL() + getRequestedURI();
    }

    /**
     * Returns the remote address which sent the request
     *
     * @return the remote address of the underlying TCP connection. This will take an X-Forwarded-For header into
     * account if the connection was opened from a known proxy ip.
     */
    public InetAddress getRemoteIP() {
        if (remoteIp == null) {
            remoteIp = WebServer.determineRemoteIP(ctx, request);
        }
        return remoteIp;
    }

    /**
     * Determines if this is an HTTPS (SSL protected) call.
     *
     * @return <tt>true</tt> if this is an HTTPS request, <tt>false</tt> otherwise
     */
    public boolean isSSL() {
        // If the request is coming from an SSL channel locally, <tt>ssl</tt> is already set true.
        if (ssl == null) {
            // Otherwise, we might sit behind an SSL offloading proxy, therefore we check
            // for the header "X-Forwarded-Proto".
            ssl = PROTOCOL_HTTPS.equalsIgnoreCase(getHeaderValue(HEADER_X_FORWARDED_PROTO).asString());
        }

        return ssl;
    }

    /**
     * Determines if the current request is secured by SSL.
     * <p>
     * This is boilerplate for: {@code CallContext.getCurrent().get(WebContext.class).isSSL()}
     *
     * @return <tt>true</tt> if this is an HTTPS request, <tt>false</tt> otherwise
     */
    public static boolean isCurrentRequestSSL() {
        return CallContext.getCurrent().get(WebContext.class).isSSL();
    }

    /**
     * Returns the query string or POST parameter with the given name.
     * <p>
     * If a POST request with query string is present, parameters in the query string have precedence.
     *
     * @param key the name of the parameter to fetch
     * @return the first value or <tt>null</tt> if the parameter was not set or empty
     */
    public String getParameter(String key) {
        return Values.of(getParameters(key)).at(0).getString();
    }

    /**
     * Returns all query string or POST parameters with the given name.
     * <p>
     * If a POST request with query string is present, parameters in the query string have precedence. If values
     * in the query string are found, the POST parameters are discarded and not added to the resulting list.
     *
     * @param key the name of the parameter to fetch
     * @return all values in the query string
     */
    public List<String> getParameters(String key) {
        if (queryString == null) {
            decodeQueryString();
        }
        if (queryString.containsKey(key)) {
            List<String> result = queryString.get(key);
            if (result == null) {
                return Collections.emptyList();
            }
            return result;
        }
        return getPostParameters(key);
    }

    private List<String> getPostParameters(String key) {
        if (postDecoder == null) {
            return Collections.emptyList();
        }
        try {
            return readPostParameters(key);
        } catch (Exception e) {
            Exceptions.handle(WebServer.LOG, e);
            return Collections.emptyList();
        }
    }

    private List<String> readPostParameters(String key) throws IOException {
        List<InterfaceHttpData> data = postDecoder.getBodyHttpDatas(key);
        if (data == null || data.isEmpty()) {
            return Collections.emptyList();
        }

        List<String> result = new ArrayList<>();
        for (InterfaceHttpData dataItem : data) {
            if (dataItem instanceof Attribute attr) {
                ByteBuf buffer = attr.getByteBuf();
                if (buffer != null) {
                    result.add(buffer.toString(attr.getCharset()));
                }
            }
        }

        return result;
    }

    /*
     * Decodes the query string on demand
     */
    private void decodeQueryString() {
        QueryStringDecoder qsd = new QueryStringDecoder(request.uri(), StandardCharsets.UTF_8);
        requestedURI = qsd.path();
        queryString = qsd.parameters();
    }

    /**
     * Strips the query part of an uri.
     *
     * @param uri the uri to remove the query string of
     * @return the uri without the query string
     */
    private String stripQueryFromURI(String uri) {
        int pathEndPos = uri.indexOf('?');
        return pathEndPos < 0 ? uri : uri.substring(0, pathEndPos);
    }

    /**
     * Overwrites the uri with the given one.
     * <p>
     * This can be used to control dispatching or to even re-dispatch a request for another destination.
     * <p>
     * Note however, that only the <tt>requestedURI</tt>, <tt>queryString</tt> and the <tt>rawRequestedURI</tt> are
     * overwritten, not the one of the underlying request.
     *
     * @param uri the new uri to use. The uri and its query string will be parsed, and the internal fields are updated
     *            accordingly.
     * @return the web context itself for fluent method calls
     */
    public WebContext withCustomURI(String uri) {
        QueryStringDecoder qsd = new QueryStringDecoder(uri, StandardCharsets.UTF_8);
        requestedURI = qsd.path();
        queryString = qsd.parameters();
        rawRequestedURI = stripQueryFromURI(uri);

        return this;
    }

    /**
     * Overwrites the path with the given one.
     * <p>
     * This can be used to control dispatching or to even re-dispatch a request for another destination.
     * <p>
     * Note however, that the original query string and its parameters remain.
     *
     * @param path the new path to use
     * @return the web context itself for fluent method calls
     */
    public WebContext withCustomPath(String path) {
        if (requestedURI == null) {
            decodeQueryString();
        }
        requestedURI = path;
        rawRequestedURI = path;

        return this;
    }

    /**
     * Returns all cookies submitted by the client
     *
     * @return a list of cookies sent by the client
     */
    public Collection<Cookie> getCookies() {
        fillCookies();
        return Collections.unmodifiableCollection(cookiesIn.values());
    }

    /**
     * Returns a cookie with the given name, sent by the client
     *
     * @param name the cookie to fetch
     * @return the client cookie with the given name, or nzl<tt>null</tt> if no matching cookie was found
     */
    public Cookie getCookie(String name) {
        fillCookies();

        return cookiesIn.get(name);
    }

    /*
     * Loads the cookies sent by the client
     */
    private void fillCookies() {
        if (cookiesIn == null) {
            cookiesIn = new HashMap<>();
            if (request != null) {
                parseCookieHeader();
            }
        }
    }

    private void parseCookieHeader() {
        String cookieHeader = request.headers().get(HttpHeaderNames.COOKIE);
        if (Strings.isFilled(cookieHeader)) {
            for (Cookie cookie : ServerCookieDecoder.LAX.decode(cookieHeader)) {
                this.cookiesIn.put(cookie.name(), cookie);
            }
        }
    }

    /**
     * Returns the data of the given client cookie wrapped as <tt>Value</tt>
     *
     * @param name the cookie to fetch
     * @return the contents of the cookie wrapped as <tt>Value</tt>
     */
    @Nullable
    public String getCookieValue(String name) {
        Cookie c = getCookie(name);
        if (c == null) {
            return null;
        }
        return c.value();
    }

    /**
     * Sets the given cookie to be sent back to the client
     *
     * @param cookie the cookie to send to the client
     */
    public void setCookie(Cookie cookie) {
        if (cookiesOut == null) {
            cookiesOut = new TreeMap<>();
        }
        cookiesOut.put(cookie.name(), cookie);
    }

    /**
     * Sets a cookie value to be sent back to the client
     * <p>
     * The generated cookie will be a session cookie and vanish once the user agent is closed
     *
     * @param name  the cookie to create
     * @param value the contents of the cookie
     */
    public void setSessionCookie(String name, String value) {
        setCookie(name, value, Long.MIN_VALUE, sessionCookieSameSite, sessionCookieSecurity);
    }

    /**
     * Sets a http only cookie value to be sent back to the client.
     * <p>
     * The generated cookie will be a session cookie and vanish once the user agent is closed. Also this cookie
     * will not be accessible by JavaScript and therefore slightly more secure.
     *
     * @param name  the cookie to create
     * @param value the contents of the cookie
     */
    public void setHTTPSessionCookie(String name, String value) {
        setCookie(name, value, Long.MIN_VALUE, sessionCookieSameSite, sessionCookieSecurity);
    }

    /**
     * Sets a cookie value to be sent back to the client.
     * <p>
     * Note that his cookie is also available to JavaScript which is inherent less secure.
     *
     * @param name          the cookie to create
     * @param value         the contents of the cookie
     * @param maxAgeSeconds contains the max age of this cookie in seconds
     * @param sameSite      the <a href="https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Set-Cookie/SameSite">SameSite</a> value
     * @param security      the policy for the security header
     */
    public void setClientCookie(String name,
                                String value,
                                long maxAgeSeconds,
                                CookieHeaderNames.SameSite sameSite,
                                CookieSecurity security) {
        setCookie(createCookie(name, value, maxAgeSeconds, sameSite, security));
    }

    /**
     * Sets a http only cookie value to be sent back to the client.
     *
     * @param name          the cookie to create
     * @param value         the contents of the cookie
     * @param maxAgeSeconds contains the max age of this cookie in seconds
     * @param sameSite      the <a href="https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Set-Cookie/SameSite">SameSite</a> value
     * @param security      the policy for the security header
     */
    public void setCookie(String name,
                          String value,
                          long maxAgeSeconds,
                          CookieHeaderNames.SameSite sameSite,
                          CookieSecurity security) {
        DefaultCookie cookie = createCookie(name, value, maxAgeSeconds, sameSite, security);
        cookie.setHttpOnly(true);
        setCookie(cookie);
    }

    private DefaultCookie createCookie(String name,
                                       String value,
                                       long maxAgeSeconds,
                                       CookieHeaderNames.SameSite sameSite,
                                       CookieSecurity security) {
        DefaultCookie cookie = new DefaultCookie(name, value);
        cookie.setMaxAge(maxAgeSeconds);
        cookie.setSameSite(sameSite);
        cookie.setSecure(security == CookieSecurity.ALWAYS_SECURE || (security == CookieSecurity.IF_SSL && isSSL()));
        cookie.setPath("/");
        if (Strings.isFilled(cookieDomain)) {
            cookie.setDomain(cookieDomain);
        }
        return cookie;
    }

    /**
     * Removes the given cookie from the cookies sent back to the client.
     *
     * @param name the cookie to delete
     */
    public void deleteCookie(@Nonnull String name) {
        setCookie(name, "", -1, null, CookieSecurity.NEVER);
    }

    /**
     * Returns all cookies to be sent to the client. Used by {@link Response} to construct an appropriate header.
     *
     * @return a list of all cookies to be sent to the client.
     */
    protected Collection<Cookie> getOutCookies(boolean cacheableRequest) {
        if (cacheableRequest) {
            // Notify a developer that changes to the client session are discarded due to a request being marked
            // as cacheable. This is treated as info, as it might be totally fine to have this behaviour if
            // the session is modified due to a side-effect...
            if (sessionModified && Sirius.isDev()) {
                WebServer.LOG.INFO("Not going to update the client session (%s) for a cacheable request: %n%s",
                                   session,
                                   this);
            }
        } else {
            buildClientSessionCookie();
        }

        return cookiesOut == null ? null : cookiesOut.values();
    }

    private void buildClientSessionCookie() {
        if (!sessionModified) {
            if (session != null && !session.isEmpty()) {
                installSessionPinningCookieIfRequired();
            }

            return;
        }

        if (session.isEmpty()) {
            if (SESSION_CHECK.isFINE()) {
                SESSION_CHECK.FINE("Deleting session for %s%nURI: %s%nIP: %s", this, getRequestedURL(), getRemoteIP());
            }

            deleteCookie(sessionCookieName);
            return;
        }

        if (SESSION_CHECK.isFINE()) {
            SESSION_CHECK.FINE("Updating session %s%n%s%nIP: %s", session, this, getRemoteIP());
        }

        QueryStringEncoder encoder = new QueryStringEncoder("");
        for (Map.Entry<String, String> e : session.entrySet()) {
            encoder.addParam(e.getKey(), e.getValue());
        }
        if (sessionCookieTTL != null) {
            encoder.addParam(TTL_SESSION_KEY, String.valueOf(sessionCookieTTL));
        }

        String value = encoder.toString();
        String protection = Hasher.sha512().hash(value + getSessionSecret(session)).toHexString();

        long ttl = determineSessionCookieTTL();
        if (ttl == 0) {
            setHTTPSessionCookie(sessionCookieName, protection + ":" + value);
        } else {
            setCookie(sessionCookieName, protection + ":" + value, ttl, sessionCookieSameSite, sessionCookieSecurity);
        }
    }

    private long determineSessionCookieTTL() {
        if (sessionCookieTTL != null) {
            return sessionCookieTTL;
        }
        return defaultSessionCookieTTL.getSeconds();
    }

    /**
     * Returns the accepted language of the client as two-letter language code.
     *
     * @return the two-letter code of the accepted language of the user agent or <tt>null</tt> if no valid accept
     * language was found
     */
    // todo: deprecate
    public Optional<String> getLang() {
        return getLanguage();
    }

    /**
     * Returns the accepted language of the client as two-letter language code.
     *
     * @return the two-letter code of the accepted language of the user agent or <tt>null</tt> if no valid accept
     * language was found
     */
    public Optional<String> getLanguage() {
        return LangHelper.from(request.headers().get(HttpHeaderNames.ACCEPT_LANGUAGE));
    }

    /*
     * Secret used to compute the protection keys for client sessions
     */
    private String getSessionSecret(Map<String, String> currentSession) {
        if (sessionSecretComputer != null) {
            return sessionSecretComputer.computeSecret(currentSession);
        }

        return getGlobalSessionSecret();
    }

    private static String getGlobalSessionSecret() {
        if (Strings.isEmpty(sessionSecret)) {
            sessionSecret = UUID.randomUUID().toString();
        }
        return sessionSecret;
    }

    /**
     * Creates a response for this request.
     *
     * @return a new response used to send data to the client.
     */
    public Response respondWith() {
        return new Response(this);
    }

    /**
     * Determines if a response was already committed.
     * <p>
     * If a response is committed a HTTP state and some headers have already been sent. Therefore, a new / other
     * response
     * cannot be created to this request.
     *
     * @return <tt>true</tt> if the response has been committed, <tt>false</tt> otherwise.
     */
    public boolean isResponseCommitted() {
        return responseCommitted;
    }

    /**
     * Returns the request header with the given name
     *
     * @param header name of the header to fetch.
     * @return the value of the given header or <tt>null</tt> if no such header is present
     */
    @Nullable
    public String getHeader(CharSequence header) {
        if (request == null) {
            return null;
        }
        return request.headers().get(header);
    }

    /**
     * Returns the request header wrapped as <tt>Value</tt>
     *
     * @param header name of the header to fetch.
     * @return the contents of the named header wrapped as <tt>Value</tt>
     */
    @Nonnull
    public Value getHeaderValue(CharSequence header) {
        if (request == null) {
            return Value.EMPTY;
        }
        return Value.of(request.headers().get(header));
    }

    /**
     * Tries to perform an HTTP Basic authentication by parsing the <tt>Authorization</tt> header.
     * <p>
     * If no such header is found or if the contents are malformed, an 401 UNAUTHORIZED response will be generated
     * ({@link Response#unauthorized(String)}) and <tt>null</tt> will be returned.
     * <p>
     * In case the username and password returned by this method are invalid, use
     * {@link Response#unauthorized(String)} to notify the client.
     *
     * @param realm the realm to report to the client in case of missing or malformed credentials
     * @return a tuple containing username and password or <tt>null</tt> to indicate that a 401 UNAUTHORIZED response
     * was sent in order to make the client send credentials.
     */
    @Nullable
    public Tuple<String, String> tryBasicAuthentication(String realm) {
        String header = getHeaderValue(HttpHeaderNames.AUTHORIZATION.toString()).asString();
        if (Strings.isFilled(header) && header.startsWith("Basic ")) {
            header = header.substring(6);
            String nameAndPassword = new String(Base64.getDecoder().decode(header), StandardCharsets.UTF_8);
            Tuple<String, String> result = Strings.split(nameAndPassword, ":");
            if (Strings.isFilled(result.getFirst()) && Strings.isFilled(result.getSecond())) {
                return result;
            }
        }
        respondWith().unauthorized(realm);
        return null;
    }

    /**
     * Returns a collection of all parameters names.
     * <p>
     * This will combine both, the query string and POST parameters.
     *
     * @return a collection of all parameters sent by the client
     */
    public Collection<String> getParameterNames() {
        if (queryString == null) {
            decodeQueryString();
        }
        Set<String> names = new LinkedHashSet<>(queryString.keySet());
        if (postDecoder != null) {
            try {
                for (InterfaceHttpData data : postDecoder.getBodyHttpDatas()) {
                    names.add(data.getName());
                }
            } catch (Exception e) {
                Exceptions.handle(WebServer.LOG, e);
            }
        }

        return names;
    }

    /**
     * Returns the original query string sent by the client.
     * <p>
     * This will not include the initial question mark.
     *
     * @return the query string (x=y&amp;z=a...) or an empty string if there is no query string
     */
    @Nonnull
    public String getQueryString() {
        String fullURI = request.uri();
        String uri = getRequestedURI();
        if (uri.length() >= fullURI.length()) {
            return "";
        }

        return request.uri().substring(uri.length() + 1);
    }

    /**
     * Returns the post decoder used to decode the posted data.
     *
     * @return the post decoder or <tt>null</tt>, if no post request is available
     */
    public InterfaceHttpPostRequestDecoder getPostDecoder() {
        return postDecoder;
    }

    /**
     * Determines if the current request is a POST request with checking for a valid CSRF-token.
     * <p>
     * A POST request signal the server to alter its state, knowing that side effects will occur.
     *
     * @return <tt>true</tt> if the method of the current request is POST and the provided CSRF-token is valid,
     * <tt>false</tt> otherwise
     */
    public boolean isSafePOST() {
        return isUnsafePOST() && checkCSRFToken();
    }

    /**
     * Determines if the current request is a POST request without checking for a valid CSRF-token.
     * <p>
     * A POST request signal the server to alter its state, knowing that side effects will occur.
     *
     * @return <tt>true</tt> if the method of the current request is POST, <tt>false</tt> otherwise
     */
    public boolean isUnsafePOST() {
        return HttpMethod.POST.equals(request.method()) && !hidePost;
    }

    /**
     * Determines if the current request is a POST request with checking for a valid CSRF-token.
     * If the token is not valid an exception is thrown in contrast to {@link #isSafePOST()}.
     * <p>
     * A POST request signal the server to alter its state, knowing that side effects will occur.
     *
     * @return <tt>true</tt> if the method of the current request is POST and the provided CSRF-token is valid,
     * <tt>false</tt> otherwise
     */
    @CheckReturnValue
    public boolean ensureSafePOST() {
        if (!isUnsafePOST()) {
            return false;
        }

        if (!checkCSRFToken()) {
            throw Exceptions.createHandled().withNLSKey("WebContext.invalidCSRFToken").handle();
        }

        return true;
    }

    private boolean checkCSRFToken() {
        String requestToken = this.get(CSRFHelper.CSRF_TOKEN).asString();
        String sessionToken = getSessionValue(CSRFHelper.CSRF_TOKEN).asString();
        String lastSessionToken = getSessionValue(CSRFHelper.PREVIOUS_CSRF_TOKEN).asString();
        return Strings.isFilled(requestToken) && (Strings.areEqual(requestToken, sessionToken) || Strings.areEqual(
                requestToken,
                lastSessionToken));
    }

    /**
     * Hide the fact that this request is a POST request.
     * <p>
     * Sometimes it is useful to make <tt>isPOST</tt> methods return false even if the
     * current request is a POST requests. Login forms would be one example. As
     * a login request is sent to any URL, we don't want a common POST handler to
     * trigger on that post data.
     */
    public void hidePost() {
        this.hidePost = true;
    }

    /*
     * Sets the post decoder used to decode the posted data
     */
    void setPostDecoder(HttpPostRequestDecoder postDecoder) {
        this.postDecoder = postDecoder;
    }

    /**
     * Provides the body of the request as stream.
     *
     * @return an input stream reading from the body of the request.
     * @throws java.io.IOException in case of an io error
     */
    public InputStream getContent() throws IOException {
        if (content == null) {
            return null;
        }
        if (!content.isInMemory()) {
            return new FileInputStream(content.getFile());
        }

        return new ByteArrayInputStream(content.get());
    }

    /**
     * Sets the charset of the body of the request.
     *
     * @param charset the charset to be applied to the body of the request
     */
    public void setContentCharset(Charset charset) {
        if (content == null) {
            return;
        }
        content.setCharset(charset);
    }

    /**
     * Returns the charset of the body of the request
     *
     * @return the charset used by the body of the request
     */
    public Charset getContentCharset() {
        if (content == null) {
            return StandardCharsets.UTF_8;
        }

        return content.getCharset();
    }

    /**
     * Returns the size in bytes of the body of the request.
     *
     * @return the size in bytes of the http body.
     */
    public long getContentSize() {
        if (contentSize == null) {
            try {
                if (content == null) {
                    contentSize = 0L;
                } else if (!content.isInMemory()) {
                    contentSize = content.getFile().length();
                } else {
                    contentSize = (long) content.getByteBuf().readableBytes();
                }
            } catch (IOException e) {
                Exceptions.handle(WebServer.LOG, e);
                return 0;
            }
        }
        return contentSize;
    }

    /**
     * Returns the content of the HTTP request as file on disk.
     * <p>
     * Note that the file will be deleted once the request is completely handled.
     *
     * @return the file pointing to the content sent by the client
     * @throws IOException in case of an IO error
     */
    public File getContentAsFile() throws IOException {
        if (content == null) {
            return null;
        }
        if (!content.isInMemory()) {
            return content.getFile();
        }
        if (contentAsFile == null) {
            contentAsFile = File.createTempFile("http", "");
            addFileToCleanup(contentAsFile);
            try (FileOutputStream outputStream = new FileOutputStream(contentAsFile)) {
                outputStream.write(content.get());
            }
        }
        return contentAsFile;
    }

    /**
     * Returns the content of the HTTP request as file on disk.
     * <p>
     * Note that the file will <b>NOT</b> be deleted once the request is completely handled. Therefore, the caller must
     * delete this file once it has been processed.
     *
     * @return the file pointing to the content sent by the client
     * @throws IOException in case of an IO error
     */
    @Nullable
    public File getContentAsCopy() throws IOException {
        if (content == null) {
            return null;
        }

        File result = File.createTempFile("http", "");
        try (FileOutputStream outputStream = new FileOutputStream(result)) {
            if (content.isInMemory()) {
                outputStream.write(content.get());
            } else {
                try (FileInputStream inputStream = new FileInputStream(content.getFile())) {
                    Streams.transfer(inputStream, outputStream);
                }
            }
        }

        return result;
    }

    /**
     * Adds a file to the cleanup list.
     * <p>
     * All files in this list will be deleted once the request is completely handled. This can be used to wipe
     * any intermediate files created while handling this request.
     *
     * @param file the file to be deleted once the request is completed.
     */
    public void addFileToCleanup(File file) {
        if (filesToCleanup == null) {
            filesToCleanup = new ArrayList<>();
        }
        filesToCleanup.add(file);
    }

    /**
     * Returns the body of the HTTP request as XML data without considering xml namespaces.
     * <p>
     * Note that all data is loaded into the heap. Therefore, certain limits apply. If the data is too large, an
     * exception will be thrown.
     * <p>
     * See: {@link #getXMLContent(boolean)} for controlling namespace awareness
     *
     * @return the body of the HTTP request as XML input
     */
    public StructuredInput getXMLContent() {
        return getXMLContent(false);
    }

    /**
     * Returns the body of the HTTP request as XML data.
     * <p>
     * Note that all data is loaded into the heap. Therefore, certain limits apply. If the data is too large, an
     * exception will be thrown.
     *
     * @param namespaceAware if true the XML will be parsed namespace aware.
     *                       See {@link DocumentBuilderFactory#setNamespaceAware(boolean)} for details.
     * @return the body of the HTTP request as XML input
     */
    public StructuredInput getXMLContent(boolean namespaceAware) {
        NamespaceContext namespaceContext = namespaceAware ? new BasicNamespaceContext() : null;
        try {
            if (content == null) {
                throw Exceptions.handle()
                                .to(WebServer.LOG)
                                .withSystemErrorMessage("Expected valid XML as body of this request.")
                                .handle();
            }
            if (content.isInMemory()) {
                try (InputStream inputStream = new ByteArrayInputStream(content.get())) {
                    return new XMLStructuredInput(inputStream, namespaceContext);
                }
            } else {
                if (content.getFile().length() > maxStructuredInputSize && maxStructuredInputSize > 0) {
                    throw Exceptions.handle()
                                    .to(WebServer.LOG)
                                    .withSystemErrorMessage(
                                            "Request body is too large to parse as XML. The limit is %d bytes",
                                            maxStructuredInputSize)
                                    .handle();
                }

                try (InputStream inputStream = new FileInputStream(content.getFile())) {
                    return new XMLStructuredInput(inputStream, namespaceContext);
                }
            }
        } catch (HandledException e) {
            throw e;
        } catch (Exception e) {
            throw Exceptions.handle()
                            .to(WebServer.LOG)
                            .error(e)
                            .withSystemErrorMessage("Expected valid XML as body of this request: %s (%s).")
                            .handle();
        }
    }

    /**
     * Returns the body of the HTTP request as JSON data.
     * <p>
     * Note that all data is loaded into the heap. Therefore, certain limits apply. If the data is too large, an
     * exception will be thrown.
     *
     * @return the body of the HTTP request as JSON input
     */
    public JSONObject getJSONContent() {
        try {
            if (content == null) {
                throw Exceptions.handle()
                                .to(WebServer.LOG)
                                .withSystemErrorMessage("Expected a valid JSON map as body of this request.")
                                .handle();
            }
            if (!content.isInMemory()
                && content.getFile().length() > maxStructuredInputSize
                && maxStructuredInputSize > 0) {
                throw Exceptions.handle()
                                .to(WebServer.LOG)
                                .withSystemErrorMessage(
                                        "Request body is too large to parse as JSON. The limit is %d bytes",
                                        maxStructuredInputSize)
                                .handle();
            }
            return JSON.parseObject(content.getString(getRequestEncoding()));
        } catch (HandledException e) {
            throw e;
        } catch (Exception e) {
            throw Exceptions.handle()
                            .to(WebServer.LOG)
                            .error(e)
                            .withSystemErrorMessage("Expected a valid JSON map as body of this request: %s (%s).")
                            .handle();
        }
    }

    /**
     * Tries to determine the charset used for the INCOMING request.
     * <p>
     * This is not to be confused with the desired charset of the outgoing data (specified via
     * <tt>Accept-Charset</tt>).
     *
     * @return the charset specified via <tt>Content-Type</tt> or <tt>UTF-8</tt> if no value is given.
     */
    public Charset getRequestEncoding() {
        try {
            Value contentType = getHeaderValue(HttpHeaderNames.CONTENT_TYPE);
            return parseContentType(contentType);
        } catch (UnsupportedCharsetException e) {
            Exceptions.ignore(e);
            return StandardCharsets.UTF_8;
        }
    }

    private Charset parseContentType(Value contentType) {
        if (contentType.isFilled()) {
            for (String property : contentType.asString().split(";")) {
                Tuple<String, String> nameValue = Strings.split(property.trim(), "=");
                if ("charset".equals(nameValue.getFirst())) {
                    return Charset.forName(nameValue.getSecond());
                }
            }
        }
        return StandardCharsets.UTF_8;
    }

    /**
     * Determines if a content is available for this request.
     *
     * @return <tt>true</tt> if content is available, <tt>false</tt> otherwise
     */
    public boolean hasContent() {
        return content != null;
    }

    /**
     * Determines if the content body might contain XML (rather than JSON).
     * <p>
     * The detection is kind of crude as we only check if the first non whitespace character is a &lt;
     *
     * @return <tt>true</tt> if the content is believed to be XML, <tt>false</tt> otherwise
     */
    public boolean isContentProbablyXML() {
        if (!hasContent()) {
            return false;
        }
        try (Reader r = new InputStreamReader(getContent())) {
            return checkIfFirstCharIsXMLBrace(r);
        } catch (HandledException e) {
            throw e;
        } catch (Exception e) {
            throw Exceptions.handle()
                            .to(WebServer.LOG)
                            .error(e)
                            .withSystemErrorMessage("Error parsing request content: %s (%s).")
                            .handle();
        }
    }

    private boolean checkIfFirstCharIsXMLBrace(Reader r) throws IOException {
        // Trim whitespace and detect if the first readable character is a <
        int c;
        while ((c = r.read()) != -1) {
            if (!Character.isWhitespace(c)) {
                return c == '<';
            }
        }
        return false;
    }

    /**
     * Releases all data associated with this request.
     */
    void release() {
        if (completionPromise != null && !completionPromise.isCompleted()) {
            completionPromise.fail(new IllegalStateException("Request has been aborted"));
        }

        releaseContentHandler();
        releasePostDecoder();
        releaseContent();
        cleanupFiles();
    }

    /**
     * Releases the content handler for a pre-dispatched request.
     * <p>
     * If the handler didn't yet read all input, all available data is drained and trashed, so that
     * the response can be sent (otherwise netty might internally hang, as it waits for the request
     * to be completely read before a (premature) response (e.g. an error message) is sent.
     * <p>
     * If no content handler is present, or if it has already been released, nothing will happen, especially nothing
     * nasty.
     */
    void releaseContentHandler() {
        if (contentHandler == null) {
            return;
        }

        try {
            ContentHandler copy = this.contentHandler;
            contentHandler = null;
            copy.cleanup();
        } catch (Exception e) {
            Exceptions.handle(WebServer.LOG, e);
        }
    }

    private void releasePostDecoder() {
        if (postDecoder == null) {
            return;
        }

        try {
            InterfaceHttpPostRequestDecoder copy = this.postDecoder;
            postDecoder = null;
            copy.destroy();
        } catch (Exception e) {
            Exceptions.handle(WebServer.LOG, e);
        }
    }

    private void releaseContent() {
        if (content == null) {
            return;
        }

        // Delete manually if anything like a file or so was allocated
        try {
            Attribute copy = this.content;
            content = null;
            contentAsFile = null;
            copy.delete();
        } catch (Exception e) {
            Exceptions.handle(WebServer.LOG, e);
        }

        // Also tell the factory to release all allocated data, as it keeps an internal reference to the request
        // (...along with all its data!).
        try {
            WebServer.getHttpDataFactory().cleanRequestHttpData(request);
        } catch (Exception e) {
            Exceptions.handle(WebServer.LOG, e);
        }
    }

    private void cleanupFiles() {
        if (filesToCleanup == null) {
            return;
        }

        for (File file : filesToCleanup) {
            Files.delete(file);
        }

        filesToCleanup = null;
    }

    /**
     * Returns {@link UserAgent} for easy access to the user agent used for this request. Also, it provides access to
     * some assumptions based on the user agent e.g. which device was used.
     *
     * @return user agent wrapper object
     */
    public UserAgent getUserAgent() {
        if (userAgent == null) {
            userAgent = new UserAgent(getHeader(HttpHeaderNames.USER_AGENT));
        }
        return userAgent;
    }

    @Override
    public String toString() {
        String result = "WebContext (Committed: " + responseCommitted + "): ";

        if (request == null) {
            return result;
        }

        return result + request;
    }

    @Override
    public SubContext fork() {
        // There is no reasonable way to clone this context. So we simply return the original instance as there
        // is only one request to answer anyway.
        return this;
    }

    @Override
    public void detach() {
        // Detaching the context from the current thread has no consequences as
        // a request cann be passed on to another thread...
    }

    /**
     * Returns the time it took from the request being scheduled for execution up until
     * the (at least) first byte of the response being sent.
     * <p>
     * At first glance this might sound like a complex metric to measure. However, this ensures
     * that only the local behaviour and duration is measured without taking system load and
     * downstream bandwidth into account.
     *
     * @return the total time in millis the system took to generate a response
     */
    public long getTTFBMillis() {
        return committed > 0 && scheduled > 0 ? committed - scheduled : 0;
    }
}
