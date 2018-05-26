/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.services;

import sirius.kernel.async.CallContext;
import sirius.kernel.health.Exceptions;
import sirius.kernel.health.HandledException;
import sirius.kernel.xml.StructuredOutput;
import sirius.web.ErrorCodeException;
import sirius.web.controller.Routed;

/**
 * Provides a service which can be called via the HTTP interface and generate a structured output encoded as JSON or
 * XML
 * <p>
 * A <tt>StructuredService</tt> must be registered using the {@link sirius.kernel.di.std.Register} annotation
 * provided with a name, which also defines the URL of the service.
 * <p>
 * The generated output can be either JSON or XML, which is completely handled by the framework.
 * <p>
 * Note: Simple AJAX calls using JSON can also be realized using the controller framework. See {@link
 * Routed#jsonCall()} for further information.
 */
public interface StructuredService {

    /**
     * Handles the incoming call while using <tt>out</tt> to generate the result.
     *
     * @param call the HTTP request to process
     * @param out  the encoder used to generate the desired output
     * @throws Exception in case of an error. An appropriate result will be generated in the selected format.
     */
    void call(ServiceCall call, StructuredOutput out) throws Exception;

    /**
     * Handles the exception caused by handling the incoming call.
     *
     * @param call the HTTP request to process
     * @param out  the encoder used to generate the desired output
     * @param e    {@link Exception} causing an error while processing the request
     */
    default void callOnError(ServiceCall call, StructuredOutput out, Exception e) {
        HandledException he = Exceptions.createHandled()
                .error(e)
                .withSystemErrorMessage("Service call to '%s' failed: %s (%s)",
                        call.getContext().getRequestedURI())
                .handle();

        out.beginResult();
        try {
            call.markCallFailed(out, he.getMessage());
            Throwable cause = e.getCause();

            while (cause != null && cause.getCause() != null && !cause.getCause().equals(cause)) {
                cause = cause.getCause();
            }

            if (cause == null) {
                cause = e;
            }

            out.property("type", cause.getClass().getName());

            if (e instanceof ErrorCodeException) {
                out.property("code", ((ErrorCodeException) e).getCode());
            } else {
                out.property("code", "ERROR");
            }

            out.property("flow", CallContext.getCurrent().getMDCValue(CallContext.MDC_FLOW));
        } finally {
            out.endResult();
        }
    }
}
