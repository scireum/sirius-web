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
import sirius.web.security.UserInfo;
import sirius.web.services.JSONStructuredOutput;

import java.io.UnsupportedEncodingException;
import java.lang.reflect.Method;
import java.net.URLDecoder;
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
    private boolean jsonCall;
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
        result.jsonCall = routed.jsonCall();
        result.preDispatchable = routed.preDispatchable();
        result.format = routed.value();
        result.permissions = Permissions.computePermissionsFromAnnotations(method);
        List<Class<?>> parameterTypes = Lists.newArrayList(Arrays.asList(method.getParameterTypes()));

        String[] elements = routed.value().split("/");
        StringBuilder finalPattern = new StringBuilder();
        int params = compileRouteURI(result, elements, finalPattern);

        if (parameterTypes.isEmpty() || !WebContext.class.equals(parameterTypes.get(0))) {
            throw new IllegalArgumentException(Strings.apply("Method needs '%s' as first parameter",
                                                             WebContext.class.getName()));
        }
        parameterTypes.remove(0);

        if (result.preDispatchable) {
            if (parameterTypes.isEmpty() || !InputStreamHandler.class.equals(parameterTypes.get(parameterTypes.size()
                                                                                                - 1))) {
                throw new IllegalArgumentException(Strings.apply("Pre-Dispatchable method needs '%s' as last parameter",
                                                                 InputStreamHandler.class.getName()));
            }
            parameterTypes.remove(parameterTypes.size() - 1);
        }
        if (result.jsonCall) {
            if (parameterTypes.isEmpty() || !JSONStructuredOutput.class.equals(parameterTypes.get(0))) {
                throw new IllegalArgumentException(Strings.apply("JSON method needs '%s' as second parameter",
                                                                 JSONStructuredOutput.class.getName()));
            }
            parameterTypes.remove(0);
        }
        if (parameterTypes.size() != params) {
            throw new IllegalArgumentException(Strings.apply("Method has %d parameters, route '%s' has %d",
                                                             parameterTypes.size(),
                                                             routed.value(),
                                                             params));
        }
        if (Strings.isEmpty(finalPattern.toString())) {
            finalPattern = new StringBuilder("/");
        }
        result.parameterTypes = parameterTypes.toArray(new Class[parameterTypes.size()]);
        result.pattern = Pattern.compile(finalPattern.toString());
        return result;
    }

    /*
     * Compiles a routed URI (which is already split into its path parts.
     * See Route.value() for a description
     */
    private static int compileRouteURI(Route result, String[] elements, StringBuilder finalPattern) {
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
        return params;
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
            if (preDispatch != this.preDispatchable) {
                return null;
            }
            Matcher m = pattern.matcher(requestedURI);
            List<Object> result = Lists.newArrayListWithCapacity(parameterTypes.length);
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
                        Object effectiveValue = Value.of(value).coerce(parameterTypes[idx - 1], null);
                        setAtPosition(result, idx, effectiveValue);
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

    /*
     * Sets the value at the given position in the list. Not that position is one-based not zero-based. If
     * the given list is too short it is padded with null values.
     */
    private void setAtPosition(List<Object> list, int position, Object value) {
        if (position == list.size() + 1) {
            list.add(value);
        } else {
            while (list.size() < position) {
                list.add(null);
            }
            list.set(position - 1, value);
        }
    }

    /**
     * Determines if the current user is authorized to access this routing.
     *
     * @return <tt>null</tt> if the user is authorized or otherwise the name of the permission which the user is
     * missing.
     */
    protected String checkAuth(UserInfo user) {
        if (permissions == null) {
            return null;
        }
        for (String p : permissions) {
            if (!user.hasPermission(p)) {
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
     * Determines if the route is pre dispatchable. In order to process requests, the last parameter of the method
     * must be of type {@link InputStreamHandler} which will be used to process the payload of the request.
     *
     * @return <tt>true</tt> if the request is pre dispatchable, <tt>false</tt> otherwise
     */
    protected boolean isPreDispatchable() {
        return preDispatchable;
    }

    /**
     * Determines if the route will result in a JSON response.
     *
     * @return the value of {@link Route#isJSONCall()} of the annotation which created this route
     */
    protected boolean isJSONCall() {
        return jsonCall;
    }
}