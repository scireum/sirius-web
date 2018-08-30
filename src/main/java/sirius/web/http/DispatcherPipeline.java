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
import sirius.kernel.di.std.Part;
import sirius.kernel.di.std.PriorityParts;
import sirius.kernel.health.Exceptions;

import java.util.Iterator;
import java.util.List;
import java.util.function.Consumer;

/**
 * Contains a dispatcher and the next in line.
 * <p>
 * Used by {@link WebServerHandler} to send a request through all dispatchers until it is handled. By using a pipeline
 * instead of a simple iteration, we can actually re-dispatch a request.
 */
class DispatcherPipeline {

    public static final String EXECUTOR_WEBSERVER = "webserver";

    @PriorityParts(WebDispatcher.class)
    private static List<WebDispatcher> dispatchers;

    @Part
    private static Tasks tasks;

    private WebDispatcher dispatcher;
    private DispatcherPipeline next;

    private DispatcherPipeline(WebDispatcher dispatcher, DispatcherPipeline next) {
        this.dispatcher = dispatcher;
        this.next = next;
    }

    private static DispatcherPipeline createStage(Iterator<WebDispatcher> iterator) {
        if (!iterator.hasNext()) {
            return null;
        }
        return new DispatcherPipeline(iterator.next(), createStage(iterator));
    }

    /**
     * Creates a fully popularized pipeline which contains all known dispatchers.
     *
     * @return a full pipeline of all dispatchers
     */
    public static DispatcherPipeline create() {
        Iterator<WebDispatcher> iterator = dispatchers.iterator();
        return createStage(iterator);
    }

    /**
     * Dispatches the given request.
     *
     * @param ctx the request to handle
     */
    public void dispatch(WebContext ctx) {
        ctx.started = System.currentTimeMillis();
        tasks.executor(EXECUTOR_WEBSERVER)
             .dropOnOverload(() -> handleDrop(ctx))
             .fork(() -> dispatch(ctx, this::dispatch, CallContext.getCurrent().get(TaskContext.class)));
    }

    private void handleDrop(WebContext ctx) {
        ctx.respondWith().error(HttpResponseStatus.INTERNAL_SERVER_ERROR, "Request dropped - System overload!");
    }

    private void dispatch(WebContext webContext, Consumer<WebContext> pipelineRoot, TaskContext context) {
        try {
            webContext.scheduled = System.currentTimeMillis();
            context.setSubSystem(dispatcher.getClass().getSimpleName());
            dispatcher.dispatch(webContext, pipelineRoot, ctx -> {
                if (next == null) {
                    ctx.respondWith().error(HttpResponseStatus.NOT_FOUND);
                } else {
                    next.dispatch(ctx, pipelineRoot, context);
                }
            });
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
            Callback<WebContext> handler = dispatcher.preparePreDispatch(webContext);
            if (handler != null) {
                tasks.executor(EXECUTOR_WEBSERVER)
                     .dropOnOverload(() -> handleDrop(webContext))
                     .fork(() -> executePreDispatching(webContext, handler));

                return true;
            }
        } catch (Exception e) {
            handleInternalServerError(webContext, e);
        }

        if (next == null) {
            return false;
        }

        return next.preDispatch(webContext);
    }

    private void executePreDispatching(WebContext webContext, Callback<WebContext> handler) {
        try {
            CallContext.getCurrent().get(TaskContext.class).setSubSystem(dispatcher.getClass().getSimpleName());
            handler.invoke(webContext);
        } catch (Exception e) {
            handleInternalServerError(webContext, e);
        }
    }
}
