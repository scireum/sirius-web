/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.services;

import io.netty.handler.codec.http.HttpResponseStatus;
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
 */
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
     * @return <tt>true</tt> if the response was already committed, <tt>false</tt> otherwise
     */
    public boolean handle(Throwable error) {
        HandledException he = Exceptions.handle()
                .to(ServiceCall.LOG)
                .error(error)
                .withSystemErrorMessage("Service call to '%s' failed: %s (%s)",
                        ctx.getRequestedURI())
                .handle();

        if (ctx.isResponseCommitted()) {
            ServiceCall.LOG.WARN("Cannot send service error for: %s. "
                            + "As a partially successful response has already been created and committed!",
                    ctx.getRequest().uri());

            // Force underlying request / response to be closed...
            ctx.respondWith().error(HttpResponseStatus.INTERNAL_SERVER_ERROR, he);
            return true;
        }

        return false;

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
        } catch (ClosedChannelException ex) {
            // If the user unexpectedly closes the connection, we do not need to log an error...
            Exceptions.ignore(ex);
        } catch (Exception t) {
            if (!handle(t)) {
                serv.callOnError(this, createOutput(), t);
            }
        }
    }

    /**
     * Creates the output used to render the result of the service call.
     *
     * @return the created output
     */
    protected abstract StructuredOutput createOutput();
}
