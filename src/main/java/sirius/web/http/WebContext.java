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
import io.netty.buffer.ByteBufInputStream;
import io.netty.channel.ChannelHandlerContext;
import io.netty.handler.codec.http.HttpHeaders;
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
import sirius.kernel.async.CallContext;
import sirius.kernel.async.SubContext;
import sirius.kernel.commons.Callback;
import sirius.kernel.commons.Strings;
import sirius.kernel.commons.Tuple;
import sirius.kernel.commons.Value;
import sirius.kernel.di.std.ConfigValue;
import sirius.kernel.di.std.Part;
import sirius.kernel.health.Exceptions;
import sirius.kernel.health.HandledException;
import sirius.kernel.nls.NLS;
import sirius.kernel.xml.StructuredInput;
import sirius.kernel.xml.XMLStructuredInput;
import sirius.web.http.session.ServerSession;
import sirius.web.http.session.SessionManager;

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
import java.util.Base64;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;

/**
 * Provides access to a request received by the WebServer.
 * <p>
 * This can be used to obtain all infos received for a HTTP request and also to create an appropriate response.
 * <p>
 * This context can either be passed along as variable or be accessed using {@link CallContext#get(Class)}
 */
public class WebContext implements SubContext {

    /**
     * Used to specify the source of a server session
     */
    public enum ServerSessionSource {
        UNKNOWN, PARAMETER, COOKIE
    }

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
    protected HttpPostRequestDecoder postDecoder;

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
     * Stores the source of the server session
     */
    private ServerSessionSource serverSessionSource;

    /*
     * Stores the requested session id
     */
    private String requestedSessionId;

    /*
     * Stores the server session once it was fetched
     */
    private ServerSession serverSession;

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
    private boolean longCall;

    /*
     * If set, will be supplied with all incoming content (instead of buffering on disk or in memory)
     */
    protected ContentHandler contentHandler;

    /*
     * Contains the timestamp this request was dispatched. (Will not be filled in predispatch, as we only
     * want to measure how long it takes to generate an "average" result, not how long an upload took....
     */
    protected long started = 0;

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
     * Parameter name in which the server session is expected
     */
    @ConfigValue("http.serverSessionParameterName")
    private static String serverSessionParameterName;

    /*
     * Cookie name used to store the server session
     */
    @ConfigValue("http.serverSessionCookieName")
    private static String serverSessionCookieName;

    /*
     * Context prefix (constant path prefix) used for this server
     */
    @ConfigValue("http.contextPrefix")
    private static String contextPrefix;

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
     * Should the automatic CORS handling be done or not?
     */
    @ConfigValue("http.corsAllowAll")
    protected static boolean corsAllowAll;

    @Part
    private static SessionManager sessionManager;

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
     * Can be set from within {@link WebDispatcher#preDispatch(WebContext)} to manually handle incoming content.
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
                return Value.of(null);
            } else {
                return Value.of(val);
            }
        }
        if (postDecoder != null) {
            try {
                InterfaceHttpData data = postDecoder.getBodyHttpData(key);
                if (data instanceof Attribute) {
                    return Value.of(((Attribute) data).getValue());
                }
            } catch (Throwable e) {
                Exceptions.handle(WebServer.LOG, e);
            }
        }
        return Value.of(null);
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
            } catch (Throwable e) {
                Exceptions.handle(WebServer.LOG, e);
            }
        }
        if (queryString == null) {
            decodeQueryString();
        }
        return queryString.containsKey(key);
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
        } catch (Throwable e) {
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
        } catch (Throwable e) {
            Exceptions.handle(WebServer.LOG, e);
        }
        return null;
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
        session = Maps.newHashMap();
        String encodedSession = getCookieValue(sessionCookieName);
        if (Strings.isFilled(encodedSession)) {
            Tuple<String, String> sessionInfo = Strings.split(encodedSession, ":");
            if (checkSessionDataIntegrity(sessionInfo)) {
                QueryStringDecoder qsd = new QueryStringDecoder(encodedSession);
                for (Map.Entry<String, List<String>> entry : qsd.parameters().entrySet()) {
                    if (TTL_SESSION_KEY.equals(entry.getKey())) {
                        sessionCookieTTL = Value.of(Iterables.getFirst(entry.getValue(), null)).getLong();
                    } else {
                        session.put(entry.getKey(), Iterables.getFirst(entry.getValue(), null));
                    }
                }
            } else {
                WebServer.LOG.FINE("Resetting client session due to security breach: %s", encodedSession);
            }
        }
    }

    private boolean checkSessionDataIntegrity(Tuple<String, String> sessionInfo) {
        return Strings.areEqual(sessionInfo.getFirst(),
                                Hashing.sha512().hashString(sessionInfo.getSecond() + getSessionSecret()).toString());
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
        session.put(key, NLS.toMachineString(value));
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
     * Returns the server sided session based on the session parameter or cookie.
     * <p>
     * If no session was found, a new one is created if create is <tt>true</tt>. Otherwise <tt>null</tt> is
     * returned.
     *
     * @param create determines if a new session should be created if no active session was found
     * @return the session associated with the client (based on session id parameter or cookie) or <tt>null</tt> if
     * neither an active session was found nor a new one was created.
     */
    public Optional<ServerSession> getServerSession(boolean create) {
        if (serverSession != null) {
            return Optional.of(serverSession);
        }

        if (serverSessionSource == null) {
            requestedSessionId = getParameter(serverSessionParameterName);
            serverSessionSource = ServerSessionSource.PARAMETER;
            if (Strings.isEmpty(requestedSessionId)) {
                serverSessionSource = ServerSessionSource.COOKIE;
                requestedSessionId = getCookieValue(serverSessionCookieName);
                if (Strings.isEmpty(requestedSessionId)) {
                    serverSessionSource = null;
                }
            }
        }

        if (Strings.isFilled(requestedSessionId)) {
            Optional<ServerSession> sessionOptional = sessionManager.getSession(requestedSessionId);
            if (sessionOptional.isPresent()) {
                serverSession = sessionOptional.get();
                return sessionOptional;
            }
        }

        if (!create) {
            return Optional.empty();
        }

        serverSession = sessionManager.create();
        serverSession.putValue(ServerSession.INITIAL_URI, getRequestedURI());
        serverSession.putValue(ServerSession.USER_AGENT, getHeader(HttpHeaders.Names.USER_AGENT));
        serverSession.putValue(ServerSession.REMOTE_IP, getRemoteIP().toString());
        return Optional.ofNullable(serverSession);
    }

    /**
     * Returns the server sided session based on the session parameter or cookie.
     * <p>
     * This method will create a new session if no active session was found.
     * <p>
     * This is a shortcut for {@code getServerSession(true)}
     *
     * @return the currently active session for this client. Will create a new session if no active session was found
     */
    public ServerSession getServerSession() {
        return getServerSession(true).get();
    }

    /**
     * Returns the session id requested by the client.
     *
     * @return the session id (server session) sent by the client.
     */
    public String getRequestedSessionId() {
        if (serverSession == null) {
            getServerSession(false);
        }
        return requestedSessionId;
    }

    /**
     * Returns the source from which the server session id was obtained.
     * <p>
     * If a session id is submitted via cookie and via parameter, the parameter always has precedence.
     *
     * @return the source from which the session id for the current server session was obtained.
     */
    public ServerSessionSource getServerSessionSource() {
        if (serverSessionSource == null && serverSession == null) {
            getServerSession(false);
        }

        return serverSessionSource;
    }

    /**
     * Returns the requested URI of the underlying HTTP request, without the query string
     *
     * @return the uri of the underlying request
     */
    public String getRequestedURI() {
        if (requestedURI == null && request != null) {
            decodeQueryString();
        }
        return requestedURI;
    }

    /**
     * Returns the remote address which sent the request
     *
     * @return the remote address of the underlying TCP connection. This will take a X-Forwarded-For header into
     * account if the connection was opened from a known proxy ip.
     */
    public InetAddress getRemoteIP() {
        if (remoteIp == null) {
            if (ctx == null || request == null) {
                try {
                    return InetAddress.getByName("127.0.0.1");
                } catch (UnknownHostException e) {
                    throw Exceptions.handle(e);
                }
            }
            remoteIp = ((InetSocketAddress) ctx.channel().remoteAddress()).getAddress();
            if (!WebServer.getProxyIPs().isEmpty()) {
                if (WebServer.getProxyIPs().accepts(remoteIp)) {
                    Value forwardedFor = Value.of(request.headers().get("X-Forwarded-For"));
                    if (forwardedFor.isFilled()) {
                        try {
                            // A X-Forwarded-For might contain many IPs like 1.2.3.4, 5.6.7.8... We're only interested
                            // in the last IP -> cut appropriately
                            Tuple<String, String> splitIPs = Strings.splitAtLast(forwardedFor.asString(), ",");
                            String forwardedForIp = Strings.isFilled(splitIPs.getSecond()) ?
                                                    splitIPs.getSecond().trim() :
                                                    splitIPs.getFirst().trim();
                            remoteIp = InetAddress.getByName(forwardedForIp);
                        } catch (Throwable e) {
                            Exceptions.ignore(e);
                            WebServer.LOG.WARN(Strings.apply(
                                    "Cannot parse X-Forwarded-For address: %s, Remote-IP: %s, Request: %s, SSL: %s - %s (%s)",
                                    forwardedFor,
                                    remoteIp,
                                    request.getUri(),
                                    NLS.toMachineString(isSSL()),
                                    e.getMessage(),
                                    e.getClass().getName()));
                        }
                    }
                }
            }
        }
        return remoteIp;
    }

    /**
     * Determines if the request is from a trusted IP.
     *
     * @return <tt>true</tt> if the request is from a trusted ip (see {@link WebServer#trustedIPs}), <tt>false</tt>
     * otherwise
     */
    public boolean isTrusted() {
        if (trusted == null) {
            if (ctx == null) {
                return true;
            }
            trusted = WebServer.getTrustedRanges().accepts(getRemoteIP());
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
            ssl = "https".equalsIgnoreCase(getHeaderValue("X-Forwarded-Proto").asString());
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
        if (postDecoder != null) {
            try {
                List<InterfaceHttpData> data = postDecoder.getBodyHttpDatas(key);
                if (data == null || data.isEmpty()) {
                    return Collections.emptyList();
                }
                List<String> result = new ArrayList<String>();
                for (InterfaceHttpData dataItem : data) {
                    if (dataItem instanceof Attribute) {
                        result.add(((Attribute) dataItem).getValue());
                    }
                }
                return result;
            } catch (Throwable e) {
                Exceptions.handle(WebServer.LOG, e);
            }
        }
        return Collections.emptyList();
    }

    /*
     * Decodes the query string on demand
     */
    private void decodeQueryString() {
        QueryStringDecoder qsd = new QueryStringDecoder(request.getUri(), Charsets.UTF_8);
        requestedURI = qsd.path();
        queryString = qsd.parameters();
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
                String cookieHeader = request.headers().get(HttpHeaders.Names.COOKIE);
                if (Strings.isFilled(cookieHeader)) {
                    for (Cookie cookie : ServerCookieDecoder.LAX.decode(cookieHeader)) {
                        this.cookiesIn.put(cookie.name(), cookie);
                    }
                }
            }
        }
    }

    /**
     * Returns the data of the given client cookie wrapped as <tt>Value</tt>
     *
     * @param name the cookie to fetch
     * @return the contents of the cookie wrapped as <tt>Value</tt>
     */
    @Nonnull
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
     * Sets a http only cookie value to be sent back to the client.
     *
     * @param name          the cookie to create
     * @param value         the contents of the cookie
     * @param maxAgeSeconds contains the max age of this cookie in seconds
     */
    public void setCookie(String name, String value, long maxAgeSeconds) {
        DefaultCookie cookie = new DefaultCookie(name, value);
        cookie.setMaxAge(maxAgeSeconds);
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
        if (serverSession != null && serverSession.isNew()) {
            setHTTPSessionCookie(serverSessionCookieName, serverSession.getId());
        }
        if (sessionModified) {
            if (session.isEmpty()) {
                deleteCookie(sessionCookieName);
            } else {
                QueryStringEncoder encoder = new QueryStringEncoder("");
                for (Map.Entry<String, String> e : session.entrySet()) {
                    encoder.addParam(e.getKey(), e.getValue());
                }
                if (sessionCookieTTL != null) {
                    encoder.addParam(TTL_SESSION_KEY, String.valueOf(sessionCookieTTL));
                }
                String value = encoder.toString();
                String protection = Hashing.sha512().hashString(value + getSessionSecret()).toString();
                long ttl = determineSessionCookieTTL();
                if (ttl == 0) {
                    setHTTPSessionCookie(sessionCookieName, protection + ":" + value);
                } else {
                    setCookie(sessionCookieName, protection + ":" + value, ttl);
                }
            }
        }
        return cookiesOut == null ? null : cookiesOut.values();
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
        String lang = CallContext.getCurrent().getLang();
        String header = getHeader(HttpHeaders.Names.ACCEPT_LANGUAGE);
        if (Strings.isEmpty(header)) {
            return lang;
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
            Locale locale = null;
            String[] l = arr[0].split("_");
            if (l.length > 0 && q > bestQ && NLS.isSupportedLanguage(l[0])) {
                lang = l[0];
                bestQ = q;
            }
        }

        return lang;
    }

    /*
     * Secret used to compute the protection keys for client sessions
     */
    private String getSessionSecret() {
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
        if (responseCommitted) {
            throw Exceptions.handle()
                            .to(WebServer.LOG)
                            .error(new IllegalStateException())
                            .withSystemErrorMessage("Response for %s was already committed!", getRequestedURI())
                            .handle();
        }
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
     * Date format used by HTTP date headers
     */
    public static final String HTTP_DATE_FORMAT = "EEE, dd MMM yyyy HH:mm:ss zzz";

    /**
     * Returns the request header with the given name
     *
     * @param header name of the header to fetch.
     * @return the value of the given header or <tt>null</tt> if no such header is present
     */
    @Nullable
    public String getHeader(String header) {
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
    public Value getHeaderValue(String header) {
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
    public long getDateHeader(String header) {
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
        String header = getHeaderValue("Authorization").asString();
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
        Set<String> names = Sets.newTreeSet(queryString.keySet());
        if (postDecoder != null) {
            try {
                for (InterfaceHttpData data : postDecoder.getBodyHttpDatas()) {
                    names.add(data.getName());
                }
            } catch (Throwable e) {
                Exceptions.handle(WebServer.LOG, e);
            }
        }

        return names;
    }

    /**
     * Returns the original query string sent by the client
     *
     * @return the query string (?x=y&amp;z=a...)
     */
    public String getQueryString() {
        return request.getUri().substring(getRequestedURI().length());
    }

    /**
     * Returns the context prefix (constant path prefix).
     * <p>
     * Can be used to let the app behave like it would be hosted in a sub directory.
     *
     * @return the content prefix or "" if no prefix is set
     */
    public static String getContextPrefix() {
        return contextPrefix;
    }

    /**
     * Returns the post decoder used to decode the posted data.
     *
     * @return the post decoder or <tt>null</tt>, if no post request is available
     */
    public HttpPostRequestDecoder getPostDecoder() {
        return postDecoder;
    }

    /**
     * Determines if the current request is a POST request.
     * <p>
     * A POST request signal the server to alter its state, knowing that side effects will occur.
     *
     * @return <tt>true</tt> if the method of the current request is POST, false otherwise
     */
    public boolean isPOST() {
        return request.getMethod() == HttpMethod.POST && !hidePost;
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
        //Backup the original size...
        contentSize = (long) content.getByteBuf().readableBytes();
        return new ByteBufInputStream(content.getByteBuf());
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

    /*
     * Caches the content size as the "readableBytes" value changes once a stream is on it.
     */
    private Long contentSize;

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
     * @deprecated Method renamed to {@link #getContentAsFile()}
     */
    @Deprecated
    public File getFileContent() throws IOException {
        return getContentAsFile();
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
        } catch (Throwable e) {
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
        } catch (Throwable e) {
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
            Value contentType = getHeaderValue(HttpHeaders.Names.CONTENT_TYPE);
            if (contentType.isFilled()) {
                for (String property : contentType.asString().split(";")) {
                    Tuple<String, String> nameValue = Strings.split(property.trim(), "=");
                    if ("charset".equals(nameValue.getFirst())) {
                        return Charset.forName(nameValue.getSecond());
                    }
                }
            }
        } catch (UnsupportedCharsetException e) {
            Exceptions.ignore(e);
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
        try {
            try (Reader r = new InputStreamReader(getContent())) {
                // Trim whitespace and detect if the first readable character is a <
                int c;
                while ((c = r.read()) != -1) {
                    if (!Character.isWhitespace(c)) {
                        return c == '<';
                    }
                }
                return false;
            }
        } catch (HandledException e) {
            throw e;
        } catch (Throwable e) {
            throw Exceptions.handle()
                            .to(WebServer.LOG)
                            .error(e)
                            .withSystemErrorMessage("Error parsing request content: %s (%s).")
                            .handle();
        }
    }

    /**
     * Releases all data associated with this request.
     */
    void release() {
        if (contentHandler != null) {
            try {
                contentHandler.cleanup();
            } catch (Exception e) {
                Exceptions.handle(WebServer.LOG, e);
            }
            contentHandler = null;
        }
        if (postDecoder != null) {
            try {
                postDecoder.cleanFiles();
            } catch (Exception e) {
                Exceptions.handle(WebServer.LOG, e);
            }
            postDecoder = null;
        }
        if (content != null) {
            // Delete manually if anything like a file or so was allocated
            try {
                content.delete();
            } catch (Exception e) {
                Exceptions.handle(WebServer.LOG, e);
            }
            // Also tell the factory to release all allocated data, as it keeps an internal reference to the request
            // (...along with all its data!).
            try {
                WebServer.getHttpDataFactory().cleanRequestHttpDatas(request);
            } catch (Exception e) {
                Exceptions.handle(WebServer.LOG, e);
            }
            content = null;
            contentAsFile = null;
        }
        if (filesToCleanup != null) {
            for (File file : filesToCleanup) {
                try {
                    if (file != null && file.exists()) {
                        file.delete();
                    }
                } catch (Exception e) {
                    Exceptions.handle(WebServer.LOG, e);
                }
            }
            filesToCleanup = null;
        }
    }

    private static final String DYNAMIC_ASSET_TOKEN = String.valueOf(System.currentTimeMillis());

    /**
     * Returns a token which can be added to dynamic asset-URLS (/asset/dynamic/TOKEN/...) to force a reload of the
     * specified resource.
     *
     * @return a random token which is guaranteed to be free of special chars (like / and the like)
     */
    public String getDynamicAssetToken() {
        return DYNAMIC_ASSET_TOKEN;
    }

    @Override
    public String toString() {
        return "WebContext (Committed: " + responseCommitted + "): " + request.toString();
    }

    @Override
    public void detach() {
        // Detaching the context from the current thread has no consequences as
        // a request cann be passed on to another thread...
    }
}
