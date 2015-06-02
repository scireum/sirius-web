/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.controller;

import com.google.common.base.Charsets;
import com.google.common.collect.Lists;
import sirius.kernel.async.CallContext;
import sirius.kernel.commons.Strings;
import sirius.kernel.commons.Tuple;
import sirius.kernel.commons.Value;
import sirius.kernel.health.Exceptions;
import sirius.kernel.nls.NLS;
import sirius.web.http.InputStreamHandler;
import sirius.web.http.WebContext;
import sirius.web.http.WebServer;
import sirius.web.security.Permissions;
import sirius.web.security.UserContext;

import java.io.UnsupportedEncodingException;
import java.lang.reflect.Method;
import java.net.URLDecoder;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Represents a compiled routed as a result of parsing a {@link Controller} and its methods.
 */
class Route {

    private static final Pattern EXPR = Pattern.compile("(:|#|\\$)\\{?(.+?)}?");

    private String format;
    private Pattern pattern;
    private List<Tuple<String, Object>> expressions = Lists.newArrayList();
    private Method successCallback;
    private String uri;
    private Class<?>[] parameterTypes;
    private Controller controller;
    private boolean preDispatchable;
    private Set<String> permissions = null;

    /**
     * Compiles a method defined by a {@link Controller}
     *
     * @param method the method wearing the Routed annotation
     * @param routed the {@link Routed} annotation
     * @return a compiled matcher handling matching URIs
     */
    protected static Route compile(Method method, Routed routed) {
        Route result = new Route();
        result.uri = routed.value();
        result.parameterTypes = method.getParameterTypes();
        result.format = routed.value();
        result.permissions = Permissions.computePermissionsFromAnnotations(method);

        String[] elements = routed.value().split("/");
        StringBuilder finalPattern = new StringBuilder();
        int params = 0;
        for (String element : elements) {
            if (Strings.isFilled(element)) {
                element = element.trim();
                element = element.replace("\n", "").replace("\t", "");
                Matcher m = EXPR.matcher(element);
                if (m.matches()) {
                    String key = m.group(1).intern();
                    if (key == ":") {
                        result.expressions.add(Tuple.create(":", Integer.parseInt(m.group(2))));
                        params++;
                    } else {
                        result.expressions.add(Tuple.create(key, m.group(2)));
                    }
                    finalPattern.append("/([^/]+)");
                } else if ("*".equals(element)) {
                    finalPattern.append("/[^/]+");
                } else if ("**".equals(element)) {
                    finalPattern.append("/?(.*)");
                    result.expressions.add(Tuple.create("**", params++));
                } else {
                    finalPattern.append("/");
                    finalPattern.append(Pattern.quote(element));
                }
            }
        }
        if (routed.preDispatchable()) {
            params++;
            if (!InputStreamHandler.class.equals(result.parameterTypes[result.parameterTypes.length - 1])) {
                throw new IllegalArgumentException(Strings.apply("Pre-Dispatchable method needs '%s' as last parameter",
                                                                 InputStreamHandler.class.getName()));
            }
        }
        if (result.parameterTypes.length - 1 != params) {
            throw new IllegalArgumentException(Strings.apply("Method has %d parameters, route '%s' has %d",
                                                             result.parameterTypes.length,
                                                             routed.value(),
                                                             params));
        }
        if (!WebContext.class.equals(result.parameterTypes[0])) {
            throw new IllegalArgumentException(Strings.apply("Method needs '%s' as first parameter",
                                                             WebContext.class.getName()));
        }
        if (Strings.isEmpty(finalPattern.toString())) {
            finalPattern = new StringBuilder("/");
        }
        result.pattern = Pattern.compile(finalPattern.toString());
        return result;
    }

    /**
     * Determines if this route matches the current request.
     *
     * @param ctx          defines the current request
     * @param requestedURI contains the request uri as string
     * @param preDispatch  determines if we're doing a pre-dispatch (looking for a controller which handles
     *                     incomplete requests like file uploads)
     * @return <tt>null</tt> if the route does not match or a list of extracted object from the URI as defined by the
     * template
     */
    protected List<Object> matches(WebContext ctx, String requestedURI, boolean preDispatch) {
        try {
            if (preDispatch && !this.preDispatchable) {
                return null;
            }
            Matcher m = pattern.matcher(requestedURI);
            List<Object> result = new ArrayList<Object>(parameterTypes.length);
            if (m.matches()) {
                for (int i = 1; i <= m.groupCount(); i++) {
                    Tuple<String, Object> expr = expressions.get(i - 1);
                    String value = URLDecoder.decode(m.group(i), Charsets.UTF_8.name());
                    if (expr.getFirst() == "$") {
                        if (!NLS.get((String) expr.getSecond()).equalsIgnoreCase(value)) {
                            return null;
                        }
                    } else if (expr.getFirst() == "#") {
                        ctx.setAttribute((String) expr.getSecond(), value);
                    } else if (expr.getFirst() == ":") {
                        int idx = (Integer) expr.getSecond();
                        if (idx == result.size() + 1) {
                            result.add(Value.of(value).coerce(parameterTypes[idx], null));
                        } else {
                            while (result.size() < idx) {
                                result.add(null);
                            }
                            result.set(idx - 1, Value.of(value).coerce(parameterTypes[idx - 1], null));
                        }
                    } else if (expr.getFirst() == "**") {
                        result.add(Arrays.asList(value.split("/")));
                    }
                }
                if (parameterTypes.length - 1 > result.size()
                    && parameterTypes[parameterTypes.length - 1] == List.class) {
                    result.add(Collections.emptyList());
                }
                CallContext.getCurrent().addToMDC("route", format);
                return result;
            }
            return null;
        } catch (UnsupportedEncodingException e) {
            throw Exceptions.handle(WebServer.LOG, e);
        }
    }

    /**
     * Determines if the current user is authorized to access this routing.
     *
     * @return <tt>null</tt> if the user is authorized or otherwise the name of the permission which the user is
     * missing.
     */
    protected String checkAuth() {
        if (permissions == null) {
            return null;
        }
        for (String p : permissions) {
            if (!UserContext.getCurrentUser().hasPermission(p)) {
                return p;
            }
        }

        return null;
    }

    @Override
    public String toString() {
        return uri;
    }

    /**
     * Returns the method which is to be invoked if the an URI can be successfully routed using this route
     * (all parameters match).
     *
     * @return the method to be invoke in order to route a request using this route
     */
    protected Method getSuccessCallback() {
        return successCallback;
    }

    /**
     * Sets the method which is used to route a request by this route.
     *
     * @param successCallback the method to be used
     */
    protected void setSuccessCallback(Method successCallback) {
        this.successCallback = successCallback;
    }

    /**
     * Sets the controller which owns (defined) this route.
     *
     * @param controller the controller owning this rout
     */
    protected void setController(Controller controller) {
        this.controller = controller;
    }

    /**
     * Returns the controller owning this route.
     *
     * @return the controller owning this route
     */
    protected Controller getController() {
        return controller;
    }

    /**
     * Sets the pre dispatchable flag of this route. This will be defined by the {@link Routed} annotation and
     * determines if this route can dispatch requests which payload was not processed yet.
     *
     * @param preDispatchable the new value for the pre-dispatchable flag
     */
    protected void setPreDispatchable(boolean preDispatchable) {
        this.preDispatchable = preDispatchable;
    }

    /**
     * Determines if the route is pre dispatchable. In order to process requests, the last parameter of the method
     * must be of type {@link InputStreamHandler} which will be used to process the payload of the request.
     *
     * @return <tt>true</tt> if the request is pre dispatchable, <tt>false</tt> otherwise
     */
    protected boolean isPreDispatchable() {
        return preDispatchable;
    }
}


