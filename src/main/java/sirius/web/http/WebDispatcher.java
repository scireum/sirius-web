/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.http;

import sirius.kernel.commons.Callback;
import sirius.kernel.di.std.Priorized;

/**
 * Participates in the dispatching process for incoming HTTP requests.
 * <p>
 * Once a HTTP request is fully read by the server it is dispatched calling all available <tt>WebDispatcher</tt>
 * instances available. To provide a WebDispatcher, a subclass therefore needs to wear an
 * {@link sirius.kernel.di.std.Register} annotation.
 * <p>
 * As the first request arrives, all dispatchers are asked for their priority (via {@link #getPriority()} and
 * then sorted according to that in an ascending manner. As default priority
 * {@link sirius.kernel.commons.PriorityCollector#DEFAULT_PRIORITY} can be used. Note that this will only be done
 * once, therefore <tt>getPriority()</tt> must only return a constant value as it is never re-evaluated. By default,
 * a not found handler ({@link sirius.web.dispatch.DefaultDispatcher} is registered with 999 as priority, so higher
 * priorities will never be executed.
 * <p>
 * For each incoming request, the list of dispatchers is iterated and {@link #dispatch(WebContext)} is invoked
 * until one of those returns <tt>true</tt>, to signal that the request was handled.
 * <p>
 * The dispatchers are invoked in a separate thread so that blocking operations will not block the web-server itself.
 * <p>
 * If a dispatcher is willing to handle all incoming data (payload of a PUT or POST request) by itself - instead of
 * just accumulating this data in memory or on disk, the {@link #preparePreDispatch(WebContext)} method must return
 * an appropriate handler for a given request. This needs to install a {@link ContentHandler}. Note that no further
 * {@link #dispatch(WebContext)} will be called for a request which received a <tt>true</tt> for its call
 * to {@link #preparePreDispatch(WebContext)}.
 * <p>
 * Note that {@link #preparePreDispatch(WebContext)} will be invoked within the event loop of the web-server, therefore
 * absolutely no blocking operations must be performed. However, the returned <tt>Callback</tt> is executed in its own
 * thread and free of any constraints.
 * </p>
 *
 * @see WebServerHandler
 */
public interface WebDispatcher extends Priorized {

    /**
     * Represents the possible outcomes of a call to a {@link WebDispatcher}.
     */
    enum DispatchDecision {
        /**
         * Signals the the request was handled and all further processing cann be aborted.
         */
        DONE,

        /**
         * Signals that the dispatcher doesn't feel responsible for this requests and that the
         * next in line dispatcher should give it a try.
         */
        CONTINUE,

        /**
         * Signals that some request re-writing happened and that the the dispatching should be
         * reset to start from the first dispatcher again.
         * <p>
         * Note that there is a circuit breaker installed, therefore this can only be performed up to
         * three times per request before the processing will be aborted.
         */
        RESTART
    }

    /**
     * Returns the priority to determine the position in the dispatcher list.
     * <p>
     * Dispatchers are sorted ascending (lower is better). The default priority is
     * {@link sirius.kernel.commons.PriorityCollector#DEFAULT_PRIORITY}, the max. value is 998 as everything above
     * will be handled by the {@link sirius.web.dispatch.DefaultDispatcher}.
     *
     * @return the priority of the dispatcher
     */
    @Override
    int getPriority();

    /**
     * Invoked as soon as the complete request but not its contents are available.
     * <p>
     * In contrast to {@link #dispatch(WebContext)} this method is invoked before the complete data is accumulated.
     * This permits the handler to install an {@link ContentHandler} using {@link WebContext#setContentHandler(ContentHandler)}
     * in order to directly process the uploaded data. A request for which <tt>true</tt> was replied will <b>not</b>
     * be dispatched again once it is complete.
     * <p>
     * Note that this method will be invoked within the event loop of the web-server, therefore absolutely no blocking
     * operations must be performed. However, the returned <tt>Callback</tt> is executed in its own thread and free of
     * any constraints.
     *
     * @param ctx the request to handle
     * @return the handler which will install the <tt>ContentHandler</tt> and eventually process the request or
     * <tt>null</tt> to indicate that no pre-dispatching is possible.
     * @see sirius.web.controller.ControllerDispatcher#preparePreDispatch(WebContext)
     */
    default Callback<WebContext> preparePreDispatch(WebContext ctx) {
        return null;
    }

    /**
     * Invoked in order to handle the given request.
     * <p>
     * If the dispatcher doesn't feel responsible for handling the request, it simply returns <tt>false</tt>. Otherwise
     * if the request is being handled, <tt>true</tt> must be returned
     * <p>
     * Note that no blocking operation must be performed in this method. For any complex interaction, a new thread
     * should be forked using {@link sirius.kernel.async.Tasks#executor(String)}. Note that even
     * {@link Response#outputStream(io.netty.handler.codec.http.HttpResponseStatus, String)} might
     * block sooner or later to limit heap memory usage - so fork a thread for any serious work besides checking
     * responsibilities for handling requests.
     *
     * @param ctx the request to handle
     * @return a decision on how to continue processing the current request
     * @throws Exception in case of an error when parsing or dispatching the request
     */
    DispatchDecision dispatch(WebContext ctx) throws Exception;
}
