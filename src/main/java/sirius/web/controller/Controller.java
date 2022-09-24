/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.controller;

import sirius.kernel.di.std.AutoRegister;
import sirius.kernel.health.ExceptionHint;
import sirius.kernel.health.HandledException;
import sirius.web.http.WebContext;
import sirius.web.services.Format;

/**
 * A {@code Controller} is responsible for handling incoming HTTP requests.
 * <p>
 * Implementing classes must wear a {@link sirius.kernel.di.std.Register} to make them visible to the MVC kernel.
 * Each class can define one to many methods wearing a {@link Routed} annotation in order to handle a specific URI.
 * Each method must have {@link WebContext} as first parameter and a string parameter for each variable in the url.
 * <p>
 * Therefore a method handling a static url looks like this:
 * <pre>
 * {@code
 *  {@literal @}Routed("/system/console")
 *  public void console(WebContext ctx) {
 *    ctx.respondWith().cached().template("view/system/console.html");
 *  }
 * }
 * </pre>
 * <p>
 * URIs can have numbered parameters like {@code /tests/:1/foos/:2}. This method needs to have two string
 * parameters (e.g. {@code void foo(WebContext ctx, String testId, String fooId)}).
 * <p>
 * If the request is not handled within the controller method (i.e. delegated to another thread pool), a
 * {@link sirius.kernel.async.Promise} must be returned which is fulfilled once the request is fully handled.
 * <p>
 * If the route being handled is a service call (which either yields a JSON or XML response), the method should
 * be annotated with either {@link sirius.web.services.InternalService} or {@link sirius.web.services.PublicService}
 * depending on the documentation needs for the created service.
 * <p>
 * To limit the access to a route, it can be annotated with either {@link sirius.web.security.LoginRequired} and/or
 * one or several {@link sirius.web.security.Permission} annotations. If a manual permission check is implemented,
 * {@link BasicController#raiseMissingPermissionError(String)} can be used to signal an authentication problem.
 * This will provide the proper HTTP response code for service calls and also will show the login page if no user
 * was present at all (which is what also happens for the annotations shown above).
 */
@AutoRegister
public interface Controller {

    /**
     * Used to specify the desired HTTP status code when handling the exception.
     */
    ExceptionHint HTTP_STATUS = new ExceptionHint("httpStatus");

    /**
     * Specifies an explicit error code to output when handling the exception.
     */
    ExceptionHint ERROR_CODE = new ExceptionHint("errorCode");

    /**
     * Specifies explicitly, that the call failed due to a missing permission (or no user being present at all).
     * <p>
     * This will be handled just like an unfulfilled {@link sirius.web.security.Permission} constraint.
     *
     * @see BasicController#raiseMissingPermissionError(String)
     */
    ExceptionHint MISSING_PERMISSION = new ExceptionHint("missingPermission");

    /**
     * In case processing a request via a method fails (throws an exception), this method will be called.
     * <p>
     * This provides a convenient way to render a "fallback" page like a list view, if a specialized details view
     * or something thereof fails.
     *
     * @param webContext   the context containing the request
     * @param error the error which occurred
     */
    void onError(WebContext webContext, HandledException error);

    /**
     * In case processing a JSON request via a method fails (throws an exception), this method will be called.
     * <p>
     * This provides a convenient way to generate a "fallback" JSON response.
     *
     * @param webContext the context containing the request
     * @param error the error which occurred
     * @param format the expected response format
     */
    void onApiError(WebContext webContext, HandledException error, Format format);

}
