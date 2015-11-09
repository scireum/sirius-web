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
     * Returns the URI pattern which describes which request should be handled. Parameters can be placed by
     * using :1, :2 etc. Therefore {@code /foo/:1} matches {@code /foo/test, /foo/hello}. For handling
     * paths with varying parts you can use /foo/:1/** which will expect a method with a signature like:
     * {@code public void foo(WebContext ctx, String param1, List&lt;String&gt; subPath)}. Such a list can
     * contain 0 to n entries.
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
     *
     * @return <tt>true</tt> if the method is used to create a JSON response for an AJAX call, <tt>false</tt> otherwise
     */
    boolean jsonCall() default false;
}
