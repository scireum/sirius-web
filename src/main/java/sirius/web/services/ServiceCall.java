/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.services;

import io.netty.handler.codec.http.HttpResponseStatus;
import sirius.kernel.async.ExecutionPoint;
import sirius.kernel.commons.Value;
import sirius.kernel.health.Exceptions;
import sirius.kernel.health.HandledException;
import sirius.kernel.health.Log;
import sirius.kernel.xml.StructuredOutput;
import sirius.web.http.WebContext;

import java.nio.channels.ClosedChannelException;
import java.util.Collections;
import java.util.List;

/**
 * Provides access to the underlying request of a call to a {@link StructuredService}
 *
 * @deprecated the whole StructuredService framework has been deprecated.
 */
@Deprecated(since = "2021/07/01")
public abstract class ServiceCall {

    protected static final Log LOG = Log.get("services");

    protected WebContext ctx;

    protected ServiceCall(WebContext ctx) {
        this.ctx = ctx;
    }

    /**
     * Handles the given exception by creating an error response.
     *
     * @param error the exception to report
     * @return {@link HandledException} the handled Exception
     */
    public HandledException handle(Throwable error) {
        return Exceptions.handle()
                         .to(ServiceCall.LOG)
                         .error(error)
                         .withSystemErrorMessage("Service call to '%s' failed: %s (%s)", ctx.getRequestedURI())
                         .handle();
    }

    /**
     * Marks the call as failed by adding the well known <tt>error</tt>, success<tt>false</tt> and <tt>message</tt>
     * properties to the given output.
     *
     * @param out     the output to write to
     * @param message the message to report
     */
    public void markCallFailed(StructuredOutput out, String message) {
        out.property("success", false);
        out.property("error", true);
        out.property("message", message);
    }

    /**
     * Marks the call as failed by adding the well known <tt>error</tt> and success<tt>false</tt>
     * properties to the given output.
     *
     * @param out the output to write to
     */
    public void markCallSuccessful(StructuredOutput out) {
        out.property("success", true);
        out.property("error", false);
    }

    /**
     * Provides access to the underlying request.
     *
     * @return the request which created the service call
     */
    public WebContext getContext() {
        return ctx;
    }

    /**
     * Returns the value provided for the given key(s).
     * <p>
     * The first non empty value is used. If all values are empty, an empty value is returned.
     *
     * @param keys the keys to check for a value
     * @return the first non empty value found for one of the given keys
     */
    public Value get(String... keys) {
        for (String key : keys) {
            Value result = ctx.get(key);
            if (result.isFilled()) {
                return result;
            }
        }
        return Value.of(null);
    }

    /**
     * Returns all query string or POST parameters provided for the given key(s).
     * <p>
     * If no parameters are found, an empty list is returned.
     *
     * @param keys the keys to check for parameters
     * @return a list of all parameters found for the given keys
     */
    public List<String> getParameters(String... keys) {
        List<String> result = Collections.emptyList();
        for (String key : keys) {
            result.addAll(ctx.getParameters(key));
        }
        return result;
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
        return ctx.require(keys);
    }

    /**
     * Calls the given service.
     *
     * @param serv the service to call
     */
    protected void invoke(StructuredService serv) {
        try {
            StructuredOutput output = createOutput();
            serv.call(this, output);
        } catch (Exception exception) {
            if (exception instanceof ClosedChannelException || exception.getCause() instanceof ClosedChannelException) {
                // If the user unexpectedly closes the connection, we do not need to log an error...
                Exceptions.ignore(exception);
                return;
            }

            HandledException handledException = handle(exception);

            if (ctx.isResponseCommitted()) {
                ServiceCall.LOG.WARN("""
                                             Cannot send service error for: %s - %s
                                             A partially successful response has already been created and committed!

                                             %s
                                             """,
                                     ctx.getRequest().uri(),
                                     exception.getMessage(),
                                     ExecutionPoint.snapshot().toString());

                // Force underlying request / response to be closed...
                ctx.respondWith().error(HttpResponseStatus.INTERNAL_SERVER_ERROR, handledException);
                return;
            }

            serv.handleException(this, createOutput(), handledException);
        }
    }

    /**
     * Creates the output used to render the result of the service call.
     *
     * @return the created output
     */
    protected abstract StructuredOutput createOutput();
}
