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
import sirius.kernel.Sirius;
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
import sirius.web.security.UserInfo;
import sirius.web.services.JSONStructuredOutput;

import java.io.UnsupportedEncodingException;
import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.reflect.Method;
import java.net.URLDecoder;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.function.Supplier;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 * Represents a compiled routed as a result of parsing a {@link Controller} and its methods.
 */
public class Route {

    protected static final List<Object> NO_MATCH = new ArrayList<>();
    private static final Class<?>[] CLASS_ARRAY = new Class[0];
    private static final MethodHandles.Lookup LOOKUP = MethodHandles.lookup();

    private static final Pattern EXPR = Pattern.compile("([:#$])\\{?(.+?)}?");

    private String label;
    private Pattern pattern;
    private List<Tuple<String, Object>> expressions = Lists.newArrayList();
    private Method method;
    private MethodHandle methodHandle;
    private String uri;
    private Class<?>[] parameterTypes;
    private Controller controller;
    private boolean preDispatchable;
    private boolean jsonCall;
    private Set<String> permissions = null;

    /**
     * Compiles a method defined by a {@link Controller}
     *
     * @param controller the controller owning the route
     * @param method     the method wearing the {@link Routed} annotation
     * @param routed     the {@link Routed} annotation
     * @return a compiled matcher handling matching URIs
     */
    protected static Route compile(Controller controller, Method method, Routed routed) {
        Route result = new Route();
        result.controller = controller;
        result.method = method;
        result.uri = applyRewrites(controller, routed.value());
        result.label = result.uri + " -> " + method.getDeclaringClass().getName() + "#" + method.getName();
        result.jsonCall = routed.jsonCall();
        result.preDispatchable = routed.preDispatchable();
        result.permissions = Permissions.computePermissionsFromAnnotations(method);
        List<Class<?>> parameterTypes = Lists.newArrayList(Arrays.asList(method.getParameterTypes()));

        if (!routed.value().startsWith("/")) {
            throw new IllegalArgumentException("Route does not start with /");
        }

        try {
            result.methodHandle = LOOKUP.unreflect(method);
        } catch (IllegalAccessException e) {
            throw new IllegalArgumentException("Callback method is not accessible!");
        }

        String[] elements = result.uri.substring(1).split("/");
        StringBuilder finalPattern = new StringBuilder();
        int params = compileRouteURI(result, elements, finalPattern);

        if (parameterTypes.isEmpty() || !WebContext.class.equals(parameterTypes.get(0))) {
            throw new IllegalArgumentException(Strings.apply("Method needs '%s' as first parameter",
                                                             WebContext.class.getName()));
        }
        parameterTypes.remove(0);

        if (result.preDispatchable) {
            failForInvalidPredispatchableMethod(parameterTypes);
            parameterTypes.remove(parameterTypes.size() - 1);
        }
        if (result.jsonCall) {
            failForInvalidJSONMethod(parameterTypes);
            parameterTypes.remove(0);
        }
        failForInvalidParameterCount(routed, parameterTypes, params);

        if (finalPattern.length() == 0) {
            finalPattern = new StringBuilder("/");
        }

        result.parameterTypes = parameterTypes.toArray(CLASS_ARRAY);
        result.pattern = Pattern.compile(finalPattern.toString());
        return result;
    }

    private static String applyRewrites(Controller controller, String uri) {
        String rewrittenUri = Sirius.getSettings()
                                    .getExtensions("controller.rewrites")
                                    .stream()
                                    .filter(ext -> controller.getClass()
                                                             .getName()
                                                             .contains(ext.get("controller").asString()))
                                    .filter(ext -> ext.get("uri").asString().equals(uri))
                                    .map(ext -> ext.get("rewrite").asString())
                                    .findFirst()
                                    .orElse(null);
        if (rewrittenUri != null) {
            ControllerDispatcher.LOG.INFO("Rewriting uri %s -> %s", uri, rewrittenUri);
            return rewrittenUri;
        }

        return uri;
    }

    private static void failForInvalidParameterCount(Routed routed, List<Class<?>> parameterTypes, int params) {
        if (parameterTypes.size() != params) {
            throw new IllegalArgumentException(Strings.apply("Method has %d parameters, route '%s' has %d",
                                                             parameterTypes.size(),
                                                             routed.value(),
                                                             params));
        }
    }

    private static void failForInvalidJSONMethod(List<Class<?>> parameterTypes) {
        if (parameterTypes.isEmpty() || !JSONStructuredOutput.class.equals(parameterTypes.get(0))) {
            throw new IllegalArgumentException(Strings.apply("JSON method needs '%s' as second parameter",
                                                             JSONStructuredOutput.class.getName()));
        }
    }

    private static void failForInvalidPredispatchableMethod(List<Class<?>> parameterTypes) {
        if (parameterTypes.isEmpty() || !InputStreamHandler.class.equals(parameterTypes.get(parameterTypes.size()
                                                                                            - 1))) {
            throw new IllegalArgumentException(Strings.apply("Pre-Dispatchable method needs '%s' as last parameter",
                                                             InputStreamHandler.class.getName()));
        }
    }

    /*
     * Compiles a routed URI (which is already split into its path parts.
     * See Route.value() for a description
     */
    private static int compileRouteURI(Route result, String[] elements, StringBuilder finalPattern) {
        int params = 0;
        for (String element : elements) {
            if (element.contains(" ") || element.contains("\n") || element.contains("\t")) {
                throw new IllegalArgumentException("A route must not contain whitespace characters!");
            }

            Matcher m = EXPR.matcher(element);
            if (m.matches()) {
                String key = m.group(1);
                if (":".equals(key)) {
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

        return params;
    }

    /**
     * Determines if this route matches the current request.
     *
     * @param ctx          defines the current request
     * @param requestedURI contains the request uri as string
     * @param preDispatch  determines if we're doing a pre-dispatch (looking for a controller which handles
     *                     incomplete requests like file uploads)
     * @return {@link #NO_MATCH} if the route does not match or a list of extracted object from the URI as defined by
     * the template
     */
    protected List<Object> matches(WebContext ctx, String requestedURI, boolean preDispatch) {
        if (preDispatch != this.preDispatchable) {
            return NO_MATCH;
        }
        Matcher m = pattern.matcher(requestedURI);
        if (m.matches()) {
            List<Object> result = extractRouteParameters(ctx, m);
            if (!result.equals(NO_MATCH)) {
                CallContext.getCurrent().addToMDC("route", uri);
            }
            return result;
        }
        return NO_MATCH;
    }

    private List<Object> extractRouteParameters(WebContext ctx, Matcher m) {
        List<Object> result = Lists.newArrayListWithCapacity(parameterTypes.length);
        for (int i = 1; i <= m.groupCount(); i++) {
            Tuple<String, Object> expr = expressions.get(i - 1);
            String value = decodeParameter(m.group(i));
            if ("$".equals(expr.getFirst())) {
                if (!NLS.get((String) expr.getSecond()).equalsIgnoreCase(value)) {
                    return NO_MATCH;
                }
            } else if ("#".equals(expr.getFirst())) {
                ctx.setAttribute((String) expr.getSecond(), value);
            } else if (":".equals(expr.getFirst())) {
                int idx = (Integer) expr.getSecond();
                Object effectiveValue = Value.of(value).coerce(parameterTypes[idx - 1], null);
                setAtPosition(result, idx, effectiveValue);
            } else if ("**".equals(expr.getFirst())) {
                //we need to split the encoded values so we dont mistake data for the delimiter
                result.add(Arrays.stream(m.group(i).split("/"))
                                 .map(this::decodeParameter)
                                 .collect(Collectors.toList()));
            }
        }
        if (parameterTypes.length - 1 > result.size() && parameterTypes[parameterTypes.length - 1] == List.class) {
            result.add(Collections.emptyList());
        }
        return result;
    }

    private String decodeParameter(String parameter) {
        try {
            return URLDecoder.decode(parameter, StandardCharsets.UTF_8.name());
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
     * @param user the current user
     * @return <tt>null</tt> if the user is authorized or otherwise the name of the permission which the user is
     * missing.
     */
    protected String checkAuth(Supplier<UserInfo> user) {
        if (permissions == null) {
            return null;
        }
        for (String p : permissions) {
            if (!user.get().hasPermission(p)) {
                return p;
            }
        }

        return null;
    }

    @Override
    public String toString() {
        return label;
    }

    /**
     * Returns the method which is to be invoked if the an URI can be successfully routed using this route
     * (all parameters match).
     *
     * @return the method to be invoke in order to route a request using this route
     */
    protected Method getMethod() {
        return method;
    }

    /**
     * Returns the controller owning this route.
     *
     * @return the controller owning this route
     */
    public Controller getController() {
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

    /**
     * Returns a string representation of the internal matching pattern, to detect routes which match the same URLs.
     *
     * @return a string representation of the internal matching pattern
     */
    protected String getPattern() {
        return pattern.toString();
    }

    /**
     * Invokes the route with the given parameters.
     *
     * @param params the parameters to supply
     * @return the result of the route. This will most probably be null (as routes are mostly void), but might be
     * a {@link Promise} which is used for JSON calls to indicate the the response will be completed in another thread.
     * @throws Exception in case of an error during the invocation
     */
    public Object invoke(List<Object> params) throws Exception {
        try {
            if (params.isEmpty()) {
                return methodHandle.invoke(controller);
            } else if (params.size() == 1) {
                return methodHandle.invoke(controller, params.get(0));
            } else if (params.size() == 2) {
                return methodHandle.invoke(controller, params.get(0), params.get(1));
            } else if (params.size() == 3) {
                return methodHandle.invoke(controller, params.get(0), params.get(1), params.get(2));
            } else if (params.size() == 4) {
                return methodHandle.invoke(controller, params.get(0), params.get(1), params.get(2), params.get(3));
            } else {
                Object[] args = new Object[params.size() + 1];
                args[0] = controller;
                for (int i = 1; i < args.length; i++) {
                    args[i] = params.get(i - 1);
                }
                return methodHandle.invokeWithArguments(args);
            }
        } catch (Exception e) {
            throw e;
        } catch (Throwable e) {
            throw Exceptions.handle()
                            .to(ControllerDispatcher.LOG)
                            .error(e)
                            .withSystemErrorMessage("A serious system error occurred when executing %s: %s (%s)", this)
                            .handle();
        }
    }
}
