/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.controller;

import sirius.kernel.commons.PriorityCollector;
import sirius.web.services.JSONStructuredOutput;

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
     * <li>a ** behaves like a variac parameter. It can only occur at the end of the expression and the method has to
     * provide a List&lt;String&gt; subPath as last parameter which will contain all matched elements</li>
     * </ul>
     *
     * @return the URI pattern describing which requests to handle
     */
    String value();

    /**
     * Determines if the annotated method supports pre-dispatching.
     * <p>
     * A pre-dispatchable method takes care of the requests payload itself. Therefore the method must declare an
     * additional parameter of type {@link sirius.web.http.InputStreamHandler} which will be used to consume
     * the data sent via POST or PUT.
     *
     * @return <tt>true</tt> if the method is pre-dispatchable, <tt>false</tt> otherwise
     */
    boolean preDispatchable() default false;

    /**
     * Determines if the annotated method is used to generate a JSON response.
     * <p>
     * This can be used to handle AJAX requests directly within a controller. In such simple cases if is often
     * feasible to keep the logic in one place (controller) instead for creating a {@link
     * sirius.web.services.StructuredService}.
     * <p>
     * A method having <tt>jsonCall</tt> set to <tt>true</tt> has to accept
     * {@link sirius.web.services.JSONStructuredOutput} as 2nd parameter. This parameter is filled with a
     * pre-initialized output writer, which has {@link JSONStructuredOutput#beginResult()} and {@link
     * JSONStructuredOutput#endResult()} automatically called.
     * <p>
     * Also the properties <tt>success</tt> and <tt>error</tt> are automatically filled. In case on an exception
     * within the controller method, a result with <tt>success</tt>, <tt>errro</tt> and <tt>message</tt> is
     * automatically created.
     * <p>
     * <b>Note:</b> If implementing method does fork a new thread and pass the given output along,
     * the method must return a {@link sirius.kernel.async.Promise} or {@link sirius.kernel.async.Future}
     * so that the dispatcher knowns when the generated output is complete.
     * </p>
     *
     * @return <tt>true</tt> if the method is used to create a JSON response for an AJAX call, <tt>false</tt> otherwise
     */
    boolean jsonCall() default false;
}
