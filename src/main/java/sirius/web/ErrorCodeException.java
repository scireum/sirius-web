/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web;

import io.netty.handler.codec.http.HttpResponseStatus;
import sirius.kernel.health.HandledException;
import sirius.web.controller.Routed;

/**
 * Can be thrown in {@link sirius.web.services.StructuredService}s or in JSON calls ({@link Routed#jsonCall()}).
 * <p>
 * Adds an additional "code" property to the JSON result which contains the given code. Also the given message
 * is directly reported without any translation or logging.
 */
public class ErrorCodeException extends HandledException {

    private static final long serialVersionUID = -5345498530492414479L;
    private final String code;
    private final HttpResponseStatus httpResponseStatus;

    /**
     * Creates a new exception with the given code and message.
     * <p>
     * Note tha this is a <tt>HandledException</tt> so no further logging or error reporting will be performed by the
     * system.
     *
     * @param code    the error code to report
     * @param message the message to add to the output
     */
    public ErrorCodeException(String code, String message) {
        super(message);
        this.code = code;
        this.httpResponseStatus = HttpResponseStatus.OK;
    }

    public ErrorCodeException(String code, String message, HttpResponseStatus httpResponseStatus) {
        super(message);
        this.code = code;
        this.httpResponseStatus = httpResponseStatus;
    }

    /**
     * Obtains the error code of this exception.
     *
     * @return the error code being reported
     */
    public String getCode() {
        return code;
    }

    public HttpResponseStatus getHttpResponseStatus() {
        return httpResponseStatus;
    }
}
