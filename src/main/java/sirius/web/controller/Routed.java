/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.controller;

import sirius.kernel.commons.PriorityCollector;
import sirius.web.services.InternalService;
import sirius.web.services.JSONStructuredOutput;
import sirius.web.services.PublicService;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Attaches a URI to a {@link Controller} method.
 * <p>
 * For conflicting URIs like {@code /foo/:1} should handle everything but {@code /foo/special}, use a
 * priority below {@link PriorityCollector#DEFAULT_PRIORITY} for {@code /foo/special}
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.METHOD)
public @interface Routed {
    /**
     * Sets the priority used for this route.
     * <p>
     * For non-conflicting URIs, this can be skipped. For everything else, the lower the better ;-)
     *
     * @return the priority used to sort the order of invocation of all rules.
     * The default is  {@link PriorityCollector#DEFAULT_PRIORITY}.
     */
    int priority() default PriorityCollector.DEFAULT_PRIORITY;

    /**
     * Returns the URI pattern which describes matching request.
     * <p>
     * The pattern should start with a / character, which is also use to separate path elements. Each element
     * can be one of the following:
     * <ul>
     * <li>a string literal like "hello". This must match the part in the request URI</li>
     * <li>a parameter index like :1. This will copy the path-element from the request into the n-th method
     * parameter</li>
     * <li>a attribute expression like #{name}: This will copy the path-element form the request into an request
     * attribute ({@link sirius.web.http.WebContext#setAttribute(String, Object)}</li>
     * <li>an i18n string like ${i18n.key}: This will be transalted using {@link sirius.kernel.nls.NLS}. The resulting
     * string must match the part in the request URI</li>
     * <li>a * can be used to accept any path element in the request URI</li>
     * <li>a ** behaves like a variadic parameter. It can only occur at the end of the expression and the method has to
     * provide a List&lt;String&gt; subPath as last parameter which will contain all matched elements</li>
     * </ul>
     *
     * @return the URI pattern describing which requests to handle
     */
    String value();

    /**
     * Determines if the annotated method supports pre-dispatching.
     * <p>
     * A pre-dispatchable method takes care of the request's payload itself. Therefore, the method must declare an
     * additional parameter of type {@link sirius.web.http.InputStreamHandler} which will be used to consume
     * the data sent via POST or PUT.
     *
     * @return <tt>true</tt> if the method is pre-dispatchable, <tt>false</tt> otherwise
     */
    boolean preDispatchable() default false;

    /**
     * Determines the HTTP methods supported by the route. By default, all methods are supported.
     * <p>
     * Note that OPTIONS is intentionally not part of the default list: it is handled centrally by the
     * {@link ControllerDispatcher} (for CORS preflight checks and to advertise the allowed methods) and only
     * reaches a controller method if it is listed here explicitly (opt-in).
     *
     * @return the supported HTTP methods
     */
    HttpMethod[] methods() default {HttpMethod.GET,
                                    HttpMethod.HEAD,
                                    HttpMethod.PUT,
                                    HttpMethod.POST,
                                    HttpMethod.CONNECT,
                                    HttpMethod.DELETE,
                                    HttpMethod.PATCH,
                                    HttpMethod.TRACE};

    /**
     * Determines if the CSRF token validation should be skipped for this route.
     * <p>
     * By default, the dispatcher validates the CSRF token for all {@link ControllerDispatcher#CSRF_VALIDATED_METHODS}
     * requests before invoking the route. Set this to <tt>true</tt> to opt out.
     *
     * @return <tt>true</tt> if CSRF validation is skipped for this route, <tt>false</tt> otherwise
     * @see Controller#isSkipCsrfValidation() for skipping CSRF validation for all routes of a specific controller
     */
    boolean skipCsrfValidation() default false;
}
