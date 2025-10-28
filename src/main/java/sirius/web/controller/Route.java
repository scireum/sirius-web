/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.controller;

import io.netty.handler.codec.http.HttpMethod;
import sirius.kernel.Sirius;
import sirius.kernel.async.CallContext;
import sirius.kernel.async.Promise;
import sirius.kernel.commons.Strings;
import sirius.kernel.commons.Tuple;
import sirius.kernel.commons.Value;
import sirius.kernel.health.Exceptions;
import sirius.kernel.nls.NLS;
import sirius.kernel.xml.XMLStructuredOutput;
import sirius.web.http.InputStreamHandler;
import sirius.web.http.WebContext;
import sirius.web.security.Permissions;
import sirius.web.security.UserInfo;
import sirius.web.services.Format;
import sirius.web.services.InternalService;
import sirius.web.services.JSONStructuredOutput;
import sirius.web.services.PublicService;

import javax.annotation.Nonnull;
import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.reflect.Method;
import java.net.URLDecoder;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.function.Supplier;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * Represents a compiled routed as a result of parsing a {@link Controller} and its methods.
 */
public class Route {

    protected static final List<Object> NO_MATCH = Collections.emptyList();
    private static final Class<?>[] CLASS_ARRAY = new Class[0];
    private static final MethodHandles.Lookup LOOKUP = MethodHandles.lookup();

    private static final Pattern EXPR = Pattern.compile("([:#$])\\{?(.+?)}?");

    private String label;
    private Pattern pattern;
    private final Set<HttpMethod> httpMethods = new HashSet<>();
    private final List<Tuple<String, Object>> expressions = new ArrayList<>();
    private Method method;
    private MethodHandle methodHandle;
    private String uri;
    private Class<?>[] parameterTypes;
    private Controller controller;
    private boolean preDispatchable;
    private Format format;
    private boolean enforceMaintenanceMode;
    private Set<String> permissions = null;
    private String subScope;
    private boolean deprecated;

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
        result.preDispatchable = routed.preDispatchable();
        result.permissions = Permissions.computePermissionsFromAnnotations(method);
        result.deprecated = method.isAnnotationPresent(Deprecated.class);

        result.httpMethods.addAll(Arrays.stream(routed.methods())
                                        .map(sirius.web.controller.HttpMethod::toHttpMethod)
                                        .toList());
        failForInvalidMethods(result.httpMethods);

        result.label = String.format("%s%s -> %s#%s",
                                     result.uri,
                                     stringifyMethods(routed.methods()).map(string -> " [" + string + "]").orElse(""),
                                     method.getDeclaringClass().getName(),
                                     method.getName());

        determineAPIFormat(method, routed, result);
        determineSubScope(method, result);
        createMethodHandle(method, result);

        List<Class<?>> parameterTypes = determineParameterTypes(method, result);

        String[] elements = result.uri.substring(1).split("/");
        StringBuilder finalPattern = new StringBuilder();
        int params = compileRouteURI(result, elements, finalPattern);
        if (finalPattern.isEmpty()) {
            finalPattern = new StringBuilder("/");
        }
        failForInvalidParameterCount(routed, parameterTypes, params);

        result.parameterTypes = parameterTypes.toArray(CLASS_ARRAY);
        result.pattern = Pattern.compile(finalPattern.toString());
        return result;
    }

    private static Optional<String> stringifyMethods(sirius.web.controller.HttpMethod[] methods) {
        // if a route supports all methods, we don't list them explicitly
        if (sirius.web.controller.HttpMethod.isCompleteList(methods)) {
            return Optional.empty();
        }
        return Optional.of(Stream.of(methods)
                                 .distinct()
                                 .map(sirius.web.controller.HttpMethod::name)
                                 .collect(Collectors.joining(", ")));
    }

    @Nonnull
    private static List<Class<?>> determineParameterTypes(Method method, Route result) {
        List<Class<?>> parameterTypes = new ArrayList<>(Arrays.asList(method.getParameterTypes()));
        if (parameterTypes.isEmpty() || !WebContext.class.equals(parameterTypes.getFirst())) {
            throw new IllegalArgumentException(Strings.apply("Method needs '%s' as first parameter",
                                                             WebContext.class.getName()));
        }
        parameterTypes.removeFirst();
        if (result.format == Format.JSON) {
            failForInvalidJSONMethod(parameterTypes);
            parameterTypes.removeFirst();
        }
        if (result.format == Format.XML) {
            failForInvalidXMLMethod(parameterTypes);
            parameterTypes.removeFirst();
        }
        if (result.preDispatchable) {
            failForInvalidPredispatchableMethod(parameterTypes);
            parameterTypes.removeLast();
        }
        return parameterTypes;
    }

    private static void createMethodHandle(Method method, Route result) {
        try {
            result.methodHandle = LOOKUP.unreflect(method);
        } catch (IllegalAccessException _) {
            throw new IllegalArgumentException("Callback method is not accessible!");
        }
    }

    private static void determineSubScope(Method method, Route result) {
        if (method.isAnnotationPresent(SubScope.class)) {
            if (result.permissions.isEmpty()) {
                ControllerDispatcher.LOG.WARN(
                        "Route %s does specify a sub scope but no permissions. The sub scope will not be enforced!",
                        result.label);
            }
            result.subScope = method.getAnnotation(SubScope.class).value();
        } else if (method.isAnnotationPresent(PublicService.class)) {
            result.subScope = SubScope.SUB_SCOPE_API;
        } else {
            result.subScope = SubScope.SUB_SCOPE_UI;
        }
    }

    @SuppressWarnings("deprecation")
    private static void determineAPIFormat(Method method, Routed routed, Route result) {
        if (method.isAnnotationPresent(PublicService.class)) {
            PublicService publicServiceAnnotation = method.getAnnotation(PublicService.class);
            result.enforceMaintenanceMode = publicServiceAnnotation.enforceMaintenanceMode();
            result.format = publicServiceAnnotation.format();
        } else if (method.isAnnotationPresent(InternalService.class)) {
            result.format = method.getAnnotation(InternalService.class).format();
        } else if (routed.jsonCall()) {
            result.format = Format.JSON;
        }
    }

    private static String applyRewrites(Controller controller, String uri) {
        if (!uri.startsWith("/")) {
            throw new IllegalArgumentException("Route does not start with /");
        }

        String rewrittenUri = Sirius.getSettings()
                                    .getExtensions("controller.rewrites")
                                    .stream()
                                    .filter(extension -> controller.getClass()
                                                                   .getName()
                                                                   .contains(extension.get("controller").asString()))
                                    .filter(extension -> extension.get("uri").asString().equals(uri))
                                    .map(extension -> extension.get("rewrite").asString())
                                    .findFirst()
                                    .orElse(null);
        if (rewrittenUri != null) {
            ControllerDispatcher.LOG.INFO("Rewriting uri %s -> %s", uri, rewrittenUri);
            return rewrittenUri;
        }

        return uri;
    }

    private static void failForInvalidMethods(Set<HttpMethod> httpMethods) {
        if (httpMethods.isEmpty()) {
            throw new IllegalArgumentException("At least one HTTP method must be specified!");
        }
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
        if (parameterTypes.isEmpty() || !JSONStructuredOutput.class.equals(parameterTypes.getFirst())) {
            throw new IllegalArgumentException(Strings.apply("JSON API method needs '%s' as second parameter",
                                                             JSONStructuredOutput.class.getName()));
        }
    }

    private static void failForInvalidXMLMethod(List<Class<?>> parameterTypes) {
        if (parameterTypes.isEmpty() || !XMLStructuredOutput.class.equals(parameterTypes.getFirst())) {
            throw new IllegalArgumentException(Strings.apply("XML API method needs '%s' as second parameter",
                                                             XMLStructuredOutput.class.getName()));
        }
    }

    private static void failForInvalidPredispatchableMethod(List<Class<?>> parameterTypes) {
        if (parameterTypes.isEmpty() || !InputStreamHandler.class.equals(parameterTypes.getLast())) {
            throw new IllegalArgumentException(Strings.apply("Pre-Dispatchable method needs '%s' as last parameter",
                                                             InputStreamHandler.class.getName()));
        }
    }

    /*
     * Compiles a routed URI (which is already split into its path parts.
     * See Route.value() for a description
     */
    private static int compileRouteURI(Route result, String[] elements, StringBuilder finalPattern) {
        int parameters = 0;
        for (String element : elements) {
            if (element.contains(" ") || element.contains("\n") || element.contains("\t")) {
                throw new IllegalArgumentException("A route must not contain whitespace characters!");
            }

            Matcher matcher = EXPR.matcher(element);
            if (matcher.matches()) {
                String key = matcher.group(1);
                if (":".equals(key)) {
                    result.expressions.add(Tuple.create(":", Integer.parseInt(matcher.group(2))));
                    parameters++;
                } else {
                    result.expressions.add(Tuple.create(key, matcher.group(2)));
                }
                finalPattern.append("/([^/]+)");
            } else if ("*".equals(element)) {
                finalPattern.append("/[^/]+");
            } else if ("**".equals(element)) {
                finalPattern.append("/?(.*)");
                result.expressions.add(Tuple.create("**", parameters++));
            } else {
                finalPattern.append("/");
                finalPattern.append(Pattern.quote(element));
            }
        }

        return parameters;
    }

    /**
     * Determines if this route matches the current request based on the request URI.
     *
     * @param webContext   defines the current request
     * @param requestedURI contains the request uri as string
     * @param preDispatch  determines if we're doing a pre-dispatch (looking for a controller which handles
     *                     incomplete requests like file uploads)
     * @return {@link #NO_MATCH} if the route does not match or a list of extracted object from the URI as defined by
     * the template
     */
    protected List<Object> matches(WebContext webContext, String requestedURI, boolean preDispatch) {
        if (preDispatch != this.preDispatchable) {
            return NO_MATCH;
        }
        Matcher matcher = pattern.matcher(requestedURI);
        if (matcher.matches()) {
            List<Object> result = extractRouteParameters(webContext, matcher);
            if (!result.equals(NO_MATCH)) {
                CallContext.getCurrent().addToMDC("route", uri);
            }
            return result;
        }
        return NO_MATCH;
    }

    /**
     * Determines if this route matches the HTTP method of the current request.
     * <p>
     * Note that this method is not included in {@link #matches(WebContext, String, boolean)} as we want to distinguish
     * between "no match at all" and "matches the URI, but not the HTTP method". The latter needs to return HTTP 405
     * (Method Not Allowed) instead of HTTP 404 (Not Found).
     *
     * @param webContext the current web context
     * @return <tt>true</tt> if the HTTP method matches, <tt>false</tt> otherwise
     */
    protected boolean matchesHttpMethod(WebContext webContext) {
        return httpMethods.contains(webContext.getRequest().method());
    }

    private List<Object> extractRouteParameters(WebContext webContext, Matcher matcher) {
        List<Object> result = new ArrayList<>(parameterTypes.length);
        for (int i = 1; i <= matcher.groupCount(); i++) {
            Tuple<String, Object> expression = expressions.get(i - 1);
            String value = decodeParameter(matcher.group(i));
            if ("$".equals(expression.getFirst())) {
                if (!NLS.get((String) expression.getSecond()).equalsIgnoreCase(value)) {
                    return NO_MATCH;
                }
            } else if ("#".equals(expression.getFirst())) {
                webContext.setAttribute((String) expression.getSecond(), value);
            } else if (":".equals(expression.getFirst())) {
                int index = (Integer) expression.getSecond();
                Object effectiveValue = Value.of(value).coerce(parameterTypes[index - 1], null);
                setAtPosition(result, index, effectiveValue);
            } else if ("**".equals(expression.getFirst())) {
                //we need to split the encoded values, so we don't mistake data for the delimiter
                result.add(Arrays.stream(matcher.group(i).split("/")).map(this::decodeParameter).toList());
            }
        }
        if (parameterTypes.length - 1 > result.size() && parameterTypes[parameterTypes.length - 1] == List.class) {
            result.add(Collections.emptyList());
        }
        return result;
    }

    private String decodeParameter(String parameter) {
        return URLDecoder.decode(parameter, StandardCharsets.UTF_8);
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

        // Note that we intentionally only check and enforce the sub scope if the
        // route is "not public accessible". This way, we do not have to fetch the
        // effective user at all for such routes...
        if (!user.get().isSubScopeEnabled(subScope)) {
            return "Scope: " + subScope;
        }

        for (String permission : permissions) {
            if (!user.get().hasPermission(permission)) {
                return permission;
            }
        }

        return null;
    }

    @Override
    public String toString() {
        return label;
    }

    /**
     * Returns the method which is to be invoked if an URI can be successfully routed using this route
     * (all parameters match).
     *
     * @return the method to be invoked in order to route a request using this route
     */
    public Method getMethod() {
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
    public boolean isPreDispatchable() {
        return preDispatchable;
    }

    /**
     * Determines if the route is a special service call which will either generate JSON or XML.
     *
     * @return <tt>true</tt> if this route is a service call, <tt>false</tt> otherwise
     */
    public boolean isServiceCall() {
        return format != null;
    }

    /**
     * Determines the response format to use if this route is an API call.
     *
     * @return the response format
     */
    public Format getApiResponseFormat() {
        return format;
    }

    /**
     * Determines if the maintenance mode is to be enforced for this route.
     *
     * @return <tt>true</tt> if this route is unreachable during maintenance, <tt>false</tt> otherwise
     */
    public boolean isEnforceMaintenanceMode() {
        return enforceMaintenanceMode;
    }

    /**
     * Determines if this route has been marked as deprecated.
     *
     * @return <tt>true</tt> if the route is deprecated, <tt>false</tt> otherwise
     */
    public boolean isDeprecated() {
        return deprecated;
    }

    /**
     * Returns a string representation of the internal matching pattern, to detect routes which match the same URLs.
     *
     * @return a string representation of the internal matching pattern
     */
    protected String getPattern() {
        return pattern.toString();
    }

    public Set<HttpMethod> getHttpMethods() {
        return Collections.unmodifiableSet(httpMethods);
    }

    public String getUri() {
        return uri;
    }

    /**
     * Invokes the route with the given parameters.
     *
     * @param parameters the parameters to supply
     * @return the result of the route. This will most probably be null (as routes are mostly void), but might be
     * a {@link Promise} which is used for JSON calls to indicate that the response will be completed in another thread.
     * @throws Exception in case of an error during the invocation
     */
    public Object invoke(List<Object> parameters) throws Exception {
        try {
            if (parameters.isEmpty()) {
                return methodHandle.invoke(controller);
            } else if (parameters.size() == 1) {
                return methodHandle.invoke(controller, parameters.getFirst());
            } else if (parameters.size() == 2) {
                return methodHandle.invoke(controller, parameters.get(0), parameters.get(1));
            } else if (parameters.size() == 3) {
                return methodHandle.invoke(controller, parameters.get(0), parameters.get(1), parameters.get(2));
            } else if (parameters.size() == 4) {
                return methodHandle.invoke(controller,
                                           parameters.get(0),
                                           parameters.get(1),
                                           parameters.get(2),
                                           parameters.get(3));
            } else {
                Object[] arguments = new Object[parameters.size() + 1];
                arguments[0] = controller;
                for (int i = 1; i < arguments.length; i++) {
                    arguments[i] = parameters.get(i - 1);
                }
                return methodHandle.invokeWithArguments(arguments);
            }
        } catch (Exception exception) {
            throw exception;
        } catch (Throwable throwable) {
            throw Exceptions.handle()
                            .to(ControllerDispatcher.LOG)
                            .error(throwable)
                            .withSystemErrorMessage("A serious system error occurred when executing %s: %s (%s)", this)
                            .handle();
        }
    }
}
