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
import com.google.common.base.Charsets;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.google.common.collect.Sets;
import com.google.common.hash.Hashing;
import com.google.common.io.Files;
import io.netty.buffer.ByteBuf;
import io.netty.buffer.ByteBufInputStream;
import io.netty.channel.ChannelHandlerContext;
import io.netty.handler.codec.http.HttpHeaderNames;
import io.netty.handler.codec.http.HttpMethod;
import io.netty.handler.codec.http.HttpRequest;
import io.netty.handler.codec.http.QueryStringDecoder;
import io.netty.handler.codec.http.QueryStringEncoder;
import io.netty.handler.codec.http.cookie.Cookie;
import io.netty.handler.codec.http.cookie.DefaultCookie;
import io.netty.handler.codec.http.cookie.ServerCookieDecoder;
import io.netty.handler.codec.http.multipart.Attribute;
import io.netty.handler.codec.http.multipart.FileUpload;
import io.netty.handler.codec.http.multipart.HttpData;
import io.netty.handler.codec.http.multipart.HttpPostRequestDecoder;
import io.netty.handler.codec.http.multipart.InterfaceHttpData;
import io.netty.handler.codec.http.multipart.InterfaceHttpPostRequestDecoder;
import sirius.kernel.async.CallContext;
import sirius.kernel.async.SubContext;
import sirius.kernel.cache.Cache;
import sirius.kernel.cache.CacheManager;
import sirius.kernel.commons.Callback;
import sirius.kernel.commons.Strings;
import sirius.kernel.commons.Tuple;
import sirius.kernel.commons.Value;
import sirius.kernel.di.std.ConfigValue;
import sirius.kernel.di.std.Part;
import sirius.kernel.health.Exceptions;
import sirius.kernel.health.HandledException;
import sirius.kernel.info.Product;
import sirius.kernel.nls.NLS;
import sirius.kernel.xml.StructuredInput;
import sirius.kernel.xml.XMLStructuredInput;
import sirius.web.controller.Message;
import sirius.web.security.UserContext;

import javax.annotation.CheckReturnValue;
import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.UnknownHostException;
import java.nio.charset.Charset;
import java.nio.charset.UnsupportedCharsetException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.Duration;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Base64;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicLong;

/**
 * Provides access to a request received by the WebServer.
 * <p>
 * This can be used to obtain all infos received for a HTTP request and also to create an appropriate response.
 * <p>
 * This context can either be passed along as variable or be accessed using {@link CallContext#get(Class)}
 */
public class WebContext implements SubContext {

    private static final String UNKNOWN_FORWARDED_FOR_HOST = "unknown";
    private static final String HEADER_X_FORWARDED_FOR = "X-Forwarded-For";
    private static final String HEADER_X_FORWARDED_PROTO = "X-Forwarded-Proto";
    private static final String PROTOCOL_HTTPS = "https";
    private static final String PROTOCOL_HTTP = "http";
    private static final String CACHED_MESSAGES_ID = "cachedMessagesId";

    /*
     * Underlying channel to send and receive data
     */
    private ChannelHandlerContext ctx;

    /*
     * Internal attributes which can be set and read back during processing. This will not contain any posted or
     * other parameters.
     */
    private Map<String, Object> attribute;

    /*
     * The underlying request created by netty
     */
    protected HttpRequest request;

    /*
     * The effective request uri (without the query string)
     */
    private String requestedURI;

    /*
     * The effective request uri (without the query string)
     */
    private String rawRequestedURI;

    /*
     * The base url (without the uri, like: http://myhost.com)
     */
    private String baseURL;

    /*
     * Contains the parameters submitted in the query string (?param=value...)
     */
    private Map<String, List<String>> queryString;

    /*
     * Contains decoded cookies which where sent by the client
     */
    private Map<String, Cookie> cookiesIn;

    /*
     * Contains cookies which will be sent to the client
     */
    protected Map<String, Cookie> cookiesOut;

    /*
     * Stores the decoder which was used to process a POST or PUT request
     */
    protected InterfaceHttpPostRequestDecoder postDecoder;

    /*
     * Sometimes it is usefult to "hide" the fact that this is a POST request.
     * One case are login-forms. There are submitted for any URL but must not
     * interact with other POST handlers. Therefore a user manager can
     * call hidePost() so that isPOST() will return false even if a post request
     * is present.
     */
    protected boolean hidePost = false;

    /*
     * A list of files to deleted once this call is handled
     */
    private List<File> filesToCleanup;

    /*
     * If the submitted data (from the client) was stored to a file, this will be stored here
     */
    private File contentAsFile;

    /*
     * Raw content submitted via POST or PUT
     */
    protected Attribute content;

    /*
     * Contains decoded data of the client session - this is sent back and forth using a cookie. This data
     * will not be stored on the server.
     */
    private Map<String, String> session;

    /*
     * Used to create IDs which are locally unique (for this web context).
     */
    private AtomicLong localIdGenerator;

    /*
     * Internal key used to keep track of the TTL of the client session cookie
     */
    private static final String TTL_SESSION_KEY = "_TTL";

    /*
     * Stores the effective session cookie TTL. If null "defaultSessionCookieTTL" is used.
     */
    private Long sessionCookieTTL;

    /*
     * Determines if the client session was modified and should be re-set via a cookie
     */
    private volatile boolean sessionModified;

    /*
     * Contains the decoded language as two-letter code
     */
    private String lang;

    /*
     * Specifies the microtiming key used for this request. If null, no microtiming will be recorded.
     */
    protected String microtimingKey;

    /*
     * Used by Response - but stored here, since a new Response might be created....
     */
    protected volatile boolean responseCommitted;

    /*
     * Used by Response - but stored here, since a new Response might be created....
     */
    protected volatile boolean responseCompleted;

    /*
     * Invoked once the call is completely handled
     */
    protected Callback<CallContext> completionCallback;

    /*
     * Determines if the requested has a trusted ip address
     */
    private Boolean trusted;

    /*
     * Determines if the request is performed via a secured channel (SSL)
     */
    protected Boolean ssl;

    /*
     * Contains the remote IP. If a proxyIP is specified (WebServer#proxyIPs), a X-Forwarded-For header is checked
     */
    private InetAddress remoteIp;

    /*
     * If longCall is set to true (by the user), the idle-state handler is disabled for this request.
     */
    private volatile boolean longCall;

    /*
     * If set, will be supplied with all incoming content (instead of buffering on disk or in memory)
     */
    protected ContentHandler contentHandler;

    /*
     * Contains the timestamp this request was dispatched. (Will not be filled in predispatch, as we only
     * want to measure how long it takes to generate an "average" result, not how long an upload took....
     */
    protected volatile long started = 0;

    /*
     * Contains the timestamp this request was scheduled for execution.
     * This can be used to measure the actual execution time without the wait time if the thread pool
     * is fully utilized and requests are queued.
     */
    protected volatile long scheduled = 0;

    /*
     * Contains the timestamp this request was commited (a response was created).
     * This can be used to actually measure the server performance and not the download speed of clients.
     */
    protected volatile long committed = 0;

    /*
     * Caches the content size as the "readableBytes" value changes once a stream is on it.
     */
    private Long contentSize;

    /*
     * Caches the user agent for this request.
     */
    protected UserAgent userAgent;

    /*
     * Name of the cookie used to store and load the client session
     */
    @ConfigValue("http.sessionCookieName")
    private static String sessionCookieName;

    /*
     * The ttl of the client session cookie. If this is 0, it will be a "session cookie" and therefore
     * be deleted when the browser is closed
     */
    @ConfigValue("http.sessionCookieTTL")
    private static Duration defaultSessionCookieTTL;

    /*
     * Shared secret used to protect the client session. If empty one will be created on startup.
     */
    @ConfigValue("http.sessionSecret")
    private static String sessionSecret;

    /*
     * Input size limit for structured data (as this is loaded into heap)
     */
    @ConfigValue("http.maxStructuredInputSize")
    private static long maxStructuredInputSize;

    /*
     * Determines if a dummy P3P header should be created to disable P3P handling.
     */
    @ConfigValue("http.addP3PHeader")
    protected static boolean addP3PHeader;

    /*
     * Determines the security policy used by the client when loading internet resources.
     */
    @ConfigValue("http.contentSecurityPolicy")
    protected static String contentSecurityPolicy;

    /*
     * Should the automatic CORS handling be done or not?
     */
    @ConfigValue("http.corsAllowAll")
    protected static boolean corsAllowAll;

    /*
     * Should a Strict-Transport-Security header be sent?
     */
    @ConfigValue("http.ssl.forceHSTS")
    protected static boolean forceHSTS;

    /*
     * Should the automatic CORS handling be done or not?
     */
    @ConfigValue("http.ssl.hstsMaxAge")
    protected static int hstsMaxAge;

    @Part
    private static SessionSecretComputer sessionSecretComputer;

    @Part
    private static DistributedUserMessageCache distributedUserMessageCache;

    private static Cache<String, List<Message>> localUserMessageCache;

    @Part
    private static CSRFHelper csrfHelper;

    /**
     * Date format used by HTTP date headers
     */
    public static final String HTTP_DATE_FORMAT = "EEE, dd MMM yyyy HH:mm:ss zzz";

    /**
     * Provides access to the underlying ChannelHandlerContext
     *
     * @return the underlying channel handler context
     */
    public ChannelHandlerContext getCtx() {
        return ctx;
    }

    /**
     * Enables microtiming for this request.
     * <p>
     * If <tt>null</tt> is passed in as key, the request uri is used.
     * <p>
     * If the microtiming was already enabled, it will remain enabled, with the original key
     *
     * @param key the key used to pass to the microtiming framework.
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
     * Note that calling this method, removes the last completion handler.
     *
     * @param onComplete the handler to be invoked once the request is completely handled
     */
    public void onComplete(Callback<CallContext> onComplete) {
        completionCallback = onComplete;
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
            return fetchPostAttribute(key);
        }

        return Value.EMPTY;
    }

    private Value fetchPostAttribute(String key) {
        try {
            InterfaceHttpData data = postDecoder.getBodyHttpData(key);
            if (data instanceof Attribute) {
                Attribute attr = (Attribute) data;
                ByteBuf byteBuf = attr.getByteBuf();

                // If the request gets aborted prematurely, the underlying buffers might
                // already be released. Therefore we have to check this here manually as
                // the server might still try to process the request...
                if (byteBuf != null) {
                    return Value.of(byteBuf.toString(attr.getCharset()));
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

    /**
     * Returns the first non empty value for the given keys.
     * <p>
     * This is a boilerplate method for {@link #get(String)} in case the same value could be sent via different
     * parameter names.
     *
     * @param keys the keys to check
     * @return the first non empty value or an empty value if no data was found for all given keys.
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
     * Returns the value provided for the given key(s) or reports an error if no non empty value was found.
     * <p>
     * The first non empty value is used. If all values are empty, an exception is thrown.
     *
     * @param keys the keys to check for a value
     * @return the first non empty value found for one of the given keys
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
            if (data instanceof HttpData) {
                return (HttpData) data;
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
            if (data instanceof FileUpload) {
                return (FileUpload) data;
            }
        } catch (Exception e) {
            Exceptions.handle(WebServer.LOG, e);
        }
        return null;
    }

    /**
     * Generates an ID (numeric value) which is unique withing this HTTP request.
     * <p>
     * This can be used to create IDs for HTML elements and the like.
     *
     * @return a locally unique ID as long as less than {@link Long#MAX_VALUE} IDs are requested.
     */
    public long generateLocalId() {
        if (localIdGenerator == null) {
            localIdGenerator = new AtomicLong(1);
        }
        return localIdGenerator.getAndIncrement();
    }

    /**
     * Sets an attribute for the current request.
     * <p>
     * Attributes are neither stored nor transmitted to the client. Therefore they are only visible during the
     * processing of this request.
     *
     * @param key   name of the attribute
     * @param value value of the attribute
     */
    public void setAttribute(String key, Object value) {
        if (attribute == null) {
            attribute = Maps.newTreeMap();
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
        } else {
            session = new HashMap<>();
        }
    }

    private Map<String, String> decodeSession(String encodedSession) {
        Tuple<String, String> sessionInfo = Strings.split(encodedSession, ":");
        Map<String, String> decodedSession = Maps.newHashMap();
        long decodedSessionTTL = -1;
        QueryStringDecoder qsd = new QueryStringDecoder(encodedSession);
        for (Map.Entry<String, List<String>> entry : qsd.parameters().entrySet()) {
            if (TTL_SESSION_KEY.equals(entry.getKey())) {
                decodedSessionTTL = Value.of(Iterables.getFirst(entry.getValue(), null)).getLong();
            } else {
                decodedSession.put(entry.getKey(), Iterables.getFirst(entry.getValue(), null));
            }
        }
        if (checkSessionDataIntegrity(decodedSession, sessionInfo)) {
            if (decodedSessionTTL > 0) {
                sessionCookieTTL = decodedSessionTTL;
            }
            return decodedSession;
        } else {
            if (WebServer.LOG.isFINE()) {
                WebServer.LOG.FINE("Resetting client session due to security breach: %s", encodedSession);
            }
            return new HashMap<>();
        }
    }

    private boolean checkSessionDataIntegrity(Map<String, String> currentSession, Tuple<String, String> sessionInfo) {
        return Strings.areEqual(sessionInfo.getFirst(),
                                Hashing.sha512()
                                       .hashString(sessionInfo.getSecond() + getSessionSecret(currentSession),
                                                   Charsets.UTF_8)
                                       .toString());
    }

    /**
     * Sets an explicit session cookie TTL (time to live).
     * <p>
     * If a non null value is given, this will overwrite {@link #defaultSessionCookieTTL} for this request/response.
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
            session.remove(key);
        } else {
            session.put(key, NLS.toMachineString(value));
        }
        sessionModified = true;
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
            return Collections.emptyList();
        }
        return Lists.newArrayList(session.keySet());
    }

    /**
     * Clears (invalidated) the client session by removing all values.
     */
    public void clearSession() {
        if (session != null) {
            session.clear();
            sessionModified = true;
        }
    }

    /**
     * Caches user messages to show them with the next request.
     * <p>
     * In some interaction patterns, we cannot directly show generated messages to a user. Therefore these are cached
     * and retrieved with the next "full" request.
     * <p>
     * When sending a redirect or performing an ajax call + a refresh, it is not possible to show messages to a user.
     * Therefore we localUserMessageCache those messages and return them with the next call to {@link UserContext#getMessages()}.
     */
    public void cacheUserMessages() {
        if (getSessionValue(CACHED_MESSAGES_ID).isFilled()) {
            return;
        }

        List<Message> messages = UserContext.get().getUserSpecificMessages();
        if (messages.isEmpty()) {
            return;
        }

        String cacheId = Strings.generateCode(32);
        if (distributedUserMessageCache != null && distributedUserMessageCache.isReady()) {
            distributedUserMessageCache.put(cacheId, messages);
        } else {
            getLocalUserMessageCache().put(cacheId, messages);
        }
        setSessionValue(CACHED_MESSAGES_ID, cacheId);
    }

    private Cache<String, List<Message>> getLocalUserMessageCache() {
        if (localUserMessageCache == null) {
            localUserMessageCache = CacheManager.createLocalCache("user-messages");
        }

        return localUserMessageCache;
    }

    /**
     * Invoked by {@link UserContext#getMessages()} to fetch and apply all previously cached message.
     */
    public void restoreCachedUserMessages() {
        if (!isValid()) {
            return;
        }

        String cachedMessagesId = getSessionValue(CACHED_MESSAGES_ID).asString();
        if (Strings.isEmpty(cachedMessagesId)) {
            return;
        }

        List<Message> cachedMessages = getAndRemoveCachedUserMessages(cachedMessagesId);
        if (cachedMessages != null) {
            cachedMessages.forEach(UserContext::message);
        }

        setSessionValue(CACHED_MESSAGES_ID, null);
    }

    private List<Message> getAndRemoveCachedUserMessages(String cachedMessagesId) {
        if (distributedUserMessageCache != null && distributedUserMessageCache.isReady()) {
            return distributedUserMessageCache.getAndRemove(cachedMessagesId);
        }

        List<Message> result = getLocalUserMessageCache().get(cachedMessagesId);
        getLocalUserMessageCache().remove(cachedMessagesId);
        return result;
    }

    /**
     * Clears all previously cached user messages
     */
    public void clearCachedUserMessages() {
        if (!isValid()) {
            return;
        }

        String cachedMessagesId = getSessionValue(CACHED_MESSAGES_ID).asString();

        if (Strings.isFilled(cachedMessagesId)) {
            getAndRemoveCachedUserMessages(cachedMessagesId);
            setSessionValue(CACHED_MESSAGES_ID, null);
        }
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
     * Returns the raw undecoded requested URI of the underlying HTTP request, without the query string
     *
     * @return the undecoded uri of the underlying request
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
     * @return the remote address of the underlying TCP connection. This will take a X-Forwarded-For header into
     * account if the connection was opened from a known proxy ip.
     */
    public InetAddress getRemoteIP() {
        if (remoteIp == null) {
            remoteIp = determineRemoteIP(ctx, request);
        }
        return remoteIp;
    }

    /**
     * Tries to determine the effective remote IP for the given context and request.
     * <p>
     * This is the remote address of the channel. However, if recognized as a proxy,
     * we use the last IP address given in the X-Forarded-For header.
     *
     * @param ctx     the channel context used to determine the physical IP
     * @param request the request used to read the appropriate headers for reverse proxies
     * @return the effective remote address of the client
     */
    public static InetAddress determineRemoteIP(ChannelHandlerContext ctx, HttpRequest request) {
        if (ctx == null || request == null) {
            try {
                return InetAddress.getLocalHost();
            } catch (UnknownHostException e) {
                throw Exceptions.handle(e);
            }
        }

        InetAddress ip = ((InetSocketAddress) ctx.channel().remoteAddress()).getAddress();
        return applyProxyIPs(ip, request);
    }

    private static InetAddress applyProxyIPs(InetAddress ip, HttpRequest request) {
        if (WebServer.getProxyIPs().isEmpty()) {
            return ip;
        }

        if (!WebServer.getProxyIPs().accepts(ip)) {
            return ip;
        }

        Value forwardedFor = Value.of(request.headers().get(HEADER_X_FORWARDED_FOR));
        if (!forwardedFor.isFilled()) {
            return ip;
        }

        try {
            // A X-Forwarded-For might contain many IPs like 1.2.3.4, 5.6.7.8... We're only interested
            // in the last IP -> cut appropriately
            Tuple<String, String> splitIPs = Strings.splitAtLast(forwardedFor.asString(), ",");
            String forwardedForIp =
                    Strings.isFilled(splitIPs.getSecond()) ? splitIPs.getSecond().trim() : splitIPs.getFirst().trim();

            // Some reverse proxies like pound use this value if an invalid X-Forwarded-For is given
            if (UNKNOWN_FORWARDED_FOR_HOST.equals(forwardedForIp)) {
                return ip;
            }

            return InetAddress.getByName(forwardedForIp);
        } catch (Exception e) {
            Exceptions.ignore(e);
            WebServer.LOG.WARN(Strings.apply(
                    "Cannot parse X-Forwarded-For address: %s, Remote-IP: %s, Request: %s - %s (%s)",
                    forwardedFor,
                    ip,
                    request.uri(),
                    e.getMessage(),
                    e.getClass().getName()));
            return ip;
        }
    }

    /**
     * Determines if the request is from a trusted IP.
     *
     * @return <tt>true</tt> if the request is from a trusted ip (see {@link WebServer#trustedIPs}), <tt>false</tt>
     * otherwise
     */
    public boolean isTrusted() {
        if (trusted == null) {
            if (ctx == null || WebServer.getTrustedRanges().isEmpty()) {
                trusted = true;
            } else {
                trusted = WebServer.getTrustedRanges().accepts(getRemoteIP());
            }
        }

        return trusted;
    }

    /**
     * Determines if this is an HTTPS (SSL protected) call.
     *
     * @return <tt>true</tt> if this is an HTTPS request, <tt>false</tt> otherwise
     */
    public boolean isSSL() {
        // If the request is coming from a SSL channel locally, <tt>ssl</tt> is already set true.
        if (ssl == null) {
            // Otherwise, we might sit behind a SSL offloading proxy, therefore we check
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
        return Iterables.getFirst(getParameters(key), null);
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
            if (dataItem instanceof Attribute) {
                Attribute attr = (Attribute) dataItem;
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
        QueryStringDecoder qsd = new QueryStringDecoder(request.uri(), Charsets.UTF_8);
        requestedURI = qsd.path();
        queryString = qsd.parameters();
    }

    /**
     * Strips the query part of a uri.
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
     * Note however, that only the the <tt>requestedURI</tt>, <tt>queryString</tt> and the <tt>rawRequestedURI</tt> are
     * overwritten, not the one of the underlying request.
     *
     * @param uri the new uri to use. The uri and its query string will be parsed and the internal fields are updated
     *            accordingly.
     * @return the web context itself for fluent method calls
     */
    public WebContext withCustomURI(String uri) {
        QueryStringDecoder qsd = new QueryStringDecoder(uri, Charsets.UTF_8);
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
        rawRequestedURI = stripQueryFromURI(path);

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
            cookiesIn = Maps.newHashMap();
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
            cookiesOut = Maps.newTreeMap();
        }
        cookiesOut.put(cookie.name(), cookie);
    }

    /**
     * Sets a cookie value to be sent back to the client
     * <p>
     * The generated cookie will be a session cookie and varnish once the user agent is closed
     *
     * @param name  the cookie to create
     * @param value the contents of the cookie
     */
    public void setSessionCookie(String name, String value) {
        setCookie(name, value, Long.MIN_VALUE);
    }

    /**
     * Sets a http only cookie value to be sent back to the client.
     * <p>
     * The generated cookie will be a session cookie and varnish once the user agent is closed. Also this cookie
     * will not be accessible by JavaScript and therefore slightly more secure.
     *
     * @param name  the cookie to create
     * @param value the contents of the cookie
     */
    public void setHTTPSessionCookie(String name, String value) {
        DefaultCookie cookie = new DefaultCookie(name, value);
        cookie.setMaxAge(Long.MIN_VALUE);
        cookie.setHttpOnly(true);
        cookie.setPath("/");
        setCookie(cookie);
    }

    /**
     * Sets a cookie value to be sent back to the client.
     * <p>
     * Note that his cookie is also available to JavaScript which is inherent less secure.
     *
     * @param name          the cookie to create
     * @param value         the contents of the cookie
     * @param maxAgeSeconds contains the max age of this cookie in seconds
     */
    public void setClientCookie(String name, String value, long maxAgeSeconds) {
        DefaultCookie cookie = new DefaultCookie(name, value);
        cookie.setMaxAge(maxAgeSeconds);
        cookie.setPath("/");
        setCookie(cookie);
    }

    /**
     * Sets a http only cookie value to be sent back to the client.
     *
     * @param name          the cookie to create
     * @param value         the contents of the cookie
     * @param maxAgeSeconds contains the max age of this cookie in seconds
     */
    public void setCookie(String name, String value, long maxAgeSeconds) {
        DefaultCookie cookie = new DefaultCookie(name, value);
        cookie.setMaxAge(maxAgeSeconds);
        cookie.setHttpOnly(true);
        cookie.setPath("/");
        setCookie(cookie);
    }

    /**
     * Removes the given cookie from the cookies sent back to the client.
     *
     * @param name the cookie to delete
     */
    public void deleteCookie(@Nonnull String name) {
        setCookie(name, "", -1);
    }

    /**
     * Returns all cookies to be sent to the client. Used by {@link Response} to construct an appropriate header.
     *
     * @return a list of all cookies to be sent to the client.
     */
    protected Collection<Cookie> getOutCookies() {
        buildClientSessionCookie();
        return cookiesOut == null ? null : cookiesOut.values();
    }

    private void buildClientSessionCookie() {
        if (!sessionModified) {
            return;
        }

        if (session.isEmpty()) {
            deleteCookie(sessionCookieName);
            return;
        }

        QueryStringEncoder encoder = new QueryStringEncoder("");
        for (Map.Entry<String, String> e : session.entrySet()) {
            encoder.addParam(e.getKey(), e.getValue());
        }
        if (sessionCookieTTL != null) {
            encoder.addParam(TTL_SESSION_KEY, String.valueOf(sessionCookieTTL));
        }

        String value = encoder.toString();
        String protection = Hashing.sha512().hashString(value + getSessionSecret(session), Charsets.UTF_8).toString();

        long ttl = determineSessionCookieTTL();
        if (ttl == 0) {
            setHTTPSessionCookie(sessionCookieName, protection + ":" + value);
        } else {
            setCookie(sessionCookieName, protection + ":" + value, ttl);
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
     * @return the two-letter code of the accepted language of the user agent. Returns the current language, if no
     * supported language was submitted.
     */
    public String getLang() {
        if (lang == null) {
            lang = parseAcceptLanguage();
        }
        return lang;
    }

    /*
     * Parses the accept language header
     */
    private String parseAcceptLanguage() {
        double bestQ = 0;
        String currentLang = CallContext.getCurrent().getLang();
        String header = getHeader(HttpHeaderNames.ACCEPT_LANGUAGE);
        if (Strings.isEmpty(header)) {
            return currentLang;
        }
        header = header.toLowerCase();
        for (String str : header.split(",")) {
            String[] arr = str.trim().replace("-", "_").split(";");

            //Parse the q-value
            double q = 1.0D;
            for (String s : arr) {
                s = s.trim();
                if (s.startsWith("q=")) {
                    q = Double.parseDouble(s.substring(2).trim());
                    break;
                }
            }

            //Parse the locale
            String[] l = arr[0].split("_");
            if (l.length > 0 && q > bestQ && NLS.isSupportedLanguage(l[0])) {
                currentLang = l[0];
                bestQ = q;
            }
        }

        return currentLang;
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
     * If a response is committed a HTTP state and some headers have already been sent. Therefore a new / other
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
     * Returns the value of a date header as UNIX timestamp in milliseconds.
     *
     * @param header the name of the header to fetch
     * @return the value in milliseconds of the submitted date or 0 if the header was not present.
     */
    public long getDateHeader(CharSequence header) {
        String value = request.headers().get(header);
        if (Strings.isEmpty(value)) {
            return 0;
        }
        try {
            SimpleDateFormat dateFormatter = new SimpleDateFormat(HTTP_DATE_FORMAT, Locale.US);
            return dateFormatter.parse(value).getTime();
        } catch (ParseException e) {
            Exceptions.ignore(e);
            return 0;
        }
    }

    /**
     * Tries to perform a HTTP Basic authentication by parsing the <tt>Authorization</tt> header.
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
            String nameAndPassword = new String(Base64.getDecoder().decode(header), Charsets.UTF_8);
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
        Set<String> names = Sets.newLinkedHashSet(queryString.keySet());
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
     * Determines if the current request is a POST request.
     * <p>
     * A POST request signal the server to alter its state, knowing that side effects will occur.
     *
     * @return <tt>true</tt> if the method of the current request is POST, false otherwise
     * @deprecated use {@link #isUnsafePOST()} and {@link #isSafePOST()} instead
     */
    @Deprecated
    public boolean isPOST() {
        return isUnsafePOST();
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
        String sessionToken = csrfHelper.getCSRFToken(this);

        return Strings.isFilled(requestToken) && Strings.areEqual(requestToken, sessionToken);
    }

    /**
     * Hide the fact that this request is a POST request.
     * <p>
     * Sometimes it is useful to make {@link #isPOST()} return false even if the
     * current request is a POST requests. Login forms woule be one example. As
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
        return new ByteBufInputStream(content.getByteBuf().copy());
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
            return Charsets.UTF_8;
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
     * Note that the file will <b>NOT</b> be deleted once the request is completely handled. Therefore the caller must
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
        if (!content.isInMemory()) {
            Files.copy(content.getFile(), result);
        } else {
            try (FileOutputStream outputStream = new FileOutputStream(result)) {
                outputStream.write(content.get());
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
            filesToCleanup = Lists.newArrayList();
        }
        filesToCleanup.add(file);
    }

    /**
     * Returns the body of the HTTP request as XML data.
     * <p>
     * Note that all data is loaded into the heap. Therefore certain limits apply. If the data is too large, an
     * exception will be thrown.
     *
     * @return the body of the HTTP request as XML input
     */
    public StructuredInput getXMLContent() {
        try {
            if (content == null) {
                throw Exceptions.handle()
                                .to(WebServer.LOG)
                                .withSystemErrorMessage("Expected valid XML as body of this request.")
                                .handle();
            }
            if (content.isInMemory()) {
                return new XMLStructuredInput(new ByteArrayInputStream(content.get()), true);
            } else {
                if (content.getFile().length() > maxStructuredInputSize && maxStructuredInputSize > 0) {
                    throw Exceptions.handle()
                                    .to(WebServer.LOG)
                                    .withSystemErrorMessage(
                                            "Request body is too large to parse as XML. The limit is %d bytes",
                                            maxStructuredInputSize)
                                    .handle();
                }
                return new XMLStructuredInput(new FileInputStream(content.getFile()), true);
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
     * Note that all data is loaded into the heap. Therefore certain limits apply. If the data is too large, an
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
            if (content.isInMemory()) {
                return JSON.parseObject(content.getString(getRequestEncoding()));
            } else {
                if (content.getFile().length() > maxStructuredInputSize && maxStructuredInputSize > 0) {
                    throw Exceptions.handle()
                                    .to(WebServer.LOG)
                                    .withSystemErrorMessage(
                                            "Request body is too large to parse as JSON. The limit is %d bytes",
                                            maxStructuredInputSize)
                                    .handle();
                }
                return JSON.parseObject(content.getString(getRequestEncoding()));
            }
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
            return Charsets.UTF_8;
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
        return Charsets.UTF_8;
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
            copy.cleanFiles();
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
            saveDeleteFile(file);
        }

        filesToCleanup = null;
    }

    private void saveDeleteFile(File file) {
        try {
            if (file != null && file.exists()) {
                if (!file.delete()) {
                    WebServer.LOG.WARN("Cannot delete temporary file: %s", file.getAbsolutePath());
                }
            }
        } catch (Exception e) {
            Exceptions.handle(WebServer.LOG, e);
        }
    }

    /**
     * Returns a token which can be added to dynamic asset-URLS (/asset/dynamic/TOKEN/...) to force a reload of the
     * specified resource.
     *
     * @return a random token which is guaranteed to be free of special chars (like / and the like)
     */
    public String getDynamicAssetToken() {
        return Product.getProduct().getUniqueVersionString();
    }

    /**
     * Returns {@link UserAgent} for easy access to the user agent used for this request. Also it provides access to
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
        return "WebContext (Committed: " + responseCommitted + "): " + request.toString();
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
}
