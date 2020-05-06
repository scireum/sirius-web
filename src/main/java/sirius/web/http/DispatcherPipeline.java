/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.http;

import io.netty.handler.codec.http.HttpResponseStatus;
import sirius.kernel.async.CallContext;
import sirius.kernel.async.TaskContext;
import sirius.kernel.async.Tasks;
import sirius.kernel.commons.Callback;
import sirius.kernel.commons.Strings;
import sirius.kernel.di.std.Part;
import sirius.kernel.di.std.PriorityParts;
import sirius.kernel.di.std.Register;
import sirius.kernel.health.Exceptions;

import java.util.List;

/**
 * Dispatches a request ({@link WebContext} by offering it to all known {@link WebDispatcher dispatchers}.
 * <p>
 * Used by {@link WebServerHandler} to send a request through all dispatchers until it is handled. Next to
 * a simple iteration, a dispatcher can always restart the pipeline to perform a "server sided redirect".
 */
@Register(classes = DispatcherPipeline.class)
public class DispatcherPipeline {

    /**
     * Specifies the executor used to handle requests.
     * <p>
     * We even call the dispatchers in a separate thread pool (out of the event loop) as otherwise
     * we might run into a deadlock due to internal buffer management within netty. Also this provides
     * quite a good circuit breaker, as the web server will start to drop load if there is a severe
     * system slowdown (which is still betten than drownin in requests, without handling any...).
     */
    private static final String EXECUTOR_WEBSERVER = "webserver";

    /**
     * Defines the max number of pipeline restarts per requests.
     * <p>
     * After that limit has been reached, processing this request will be aborted.
     */
    private static final int MAX_RESTARTS_PER_REQUEST = 3;

    @PriorityParts(WebDispatcher.class)
    private List<WebDispatcher> dispatchers;

    @Part
    private Tasks tasks;

    /**
     * Dispatches the given request.
     *
     * @param ctx the request to handle
     */
    public void dispatch(WebContext ctx) {
        ctx.started = System.currentTimeMillis();
        tasks.executor(EXECUTOR_WEBSERVER)
             .dropOnOverload(() -> handleDrop(ctx))
             .fork(() -> dispatch(ctx, CallContext.getCurrent().get(TaskContext.class)));
    }

    private void handleDrop(WebContext ctx) {
        ctx.respondWith().error(HttpResponseStatus.INTERNAL_SERVER_ERROR, "Request dropped - System overload!");
    }

    private void dispatch(WebContext webContext, TaskContext context) {
        try {
            webContext.scheduled = System.currentTimeMillis();
            int leftTries = MAX_RESTARTS_PER_REQUEST * dispatchers.size();
            int index = 0;
            while (index < dispatchers.size()) {
                WebDispatcher dispatcher = dispatchers.get(index);
                context.setSubSystem(dispatcher.getClass().getSimpleName());
                WebDispatcher.DispatchDecision decision = dispatcher.dispatch(webContext);
                if (decision == WebDispatcher.DispatchDecision.RESTART) {
                    index = 0;
                } else if (decision == WebDispatcher.DispatchDecision.CONTINUE) {
                    index++;
                } else if (decision == WebDispatcher.DispatchDecision.DONE) {
                    return;
                }

                if (leftTries-- <= 0) {
                    throw new IllegalStateException(Strings.apply(
                            "Dispatcher pipeline restarted more than %s times for %s, aborting...",
                            MAX_RESTARTS_PER_REQUEST,
                            webContext.getRequestedURL()));
                }
            }
        } catch (Exception e) {
            handleInternalServerError(webContext, e);
        }
    }

    private void handleInternalServerError(WebContext webContext, Exception e) {
        if (webContext.responseCommitted) {
            return;
        }

        webContext.respondWith().error(HttpResponseStatus.INTERNAL_SERVER_ERROR, Exceptions.handle(WebServer.LOG, e));
    }

    /**
     * Pre-dispatches the given request.
     *
     * @param webContext the request to handle
     * @return <tt>true</tt> if the request was handled, <tt>false</tt> otherwise
     */
    public boolean preDispatch(WebContext webContext) {
        try {
            for (WebDispatcher webDispatcher : dispatchers) {
                Callback<WebContext> handler = webDispatcher.preparePreDispatch(webContext);
                if (handler != null) {
                    tasks.executor(EXECUTOR_WEBSERVER)
                         .dropOnOverload(() -> handleDrop(webContext))
                         .fork(() -> executePreDispatching(webContext, webDispatcher, handler));

                    return true;
                }
            }
        } catch (Exception e) {
            handleInternalServerError(webContext, e);
        }

        return false;
    }

    private void executePreDispatching(WebContext webContext, WebDispatcher dispatcher, Callback<WebContext> handler) {
        try {
            CallContext.getCurrent().get(TaskContext.class).setSubSystem(dispatcher.getClass().getSimpleName());
            handler.invoke(webContext);
        } catch (Exception e) {
            handleInternalServerError(webContext, e);
        }
    }
}
