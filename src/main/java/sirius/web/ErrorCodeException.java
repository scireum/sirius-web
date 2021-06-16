/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web;

import io.netty.handler.codec.http.HttpResponseStatus;
import sirius.kernel.commons.Tuple;
import sirius.kernel.health.ExceptionHint;
import sirius.kernel.health.HandledException;
import sirius.web.controller.Controller;
import sirius.web.controller.Routed;

import java.io.Serial;
import java.util.Collections;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * Can be thrown in {@link sirius.web.services.StructuredService}s or in JSON calls ({@link Routed#jsonCall()}).
 * <p>
 * Adds an additional "code" property to the JSON result which contains the given code. Also the given message
 * is directly reported without any translation or logging.
 *
 * @deprecated Use {@link sirius.kernel.health.Exceptions.ErrorHandler#hint(ExceptionHint, Object)} and
 * {@link sirius.web.controller.Controller#HTTP_STATUS} or {@link sirius.web.controller.Controller#ERROR_CODE}.
 */
@Deprecated
public class ErrorCodeException extends HandledException {

    @Serial
    private static final long serialVersionUID = -5345498530492414479L;

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
        super(message, Collections.singletonMap(Controller.ERROR_CODE, code), null);
    }

    /**
     * Creates a new exception with the given code, message and {@link HttpResponseStatus}
     * <p>
     * Note tha this is a <tt>HandledException</tt> so no further logging or error reporting will be performed by the
     * system.
     *
     * @param code               the error code to report
     * @param message            the message to add to the output
     * @param httpResponseStatus the {@link HttpResponseStatus} to report
     */
    public ErrorCodeException(String code, String message, HttpResponseStatus httpResponseStatus) {
        super(message,
              Stream.of(Tuple.create(Controller.ERROR_CODE, code),
                        Tuple.create(Controller.HTTP_STATUS, httpResponseStatus.code()))
                    .collect(Collectors.toMap(Tuple::getFirst, Tuple::getSecond)),
              null);
    }

    /**
     * Obtains the error code of this exception.
     *
     * @return the error code being reported
     */
    public String getCode() {
        return getHint(Controller.ERROR_CODE).asString();
    }

    public HttpResponseStatus getHttpResponseStatus() {
        return HttpResponseStatus.valueOf(getHint(Controller.HTTP_STATUS).asInt(200));
    }
}
