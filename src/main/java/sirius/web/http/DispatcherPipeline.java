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

    @PriorityParts(WebDispatcher.class)
    private static List<WebDispatcher> dispatchers;

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
        dispatch(ctx, this::dispatch, CallContext.getCurrent().get(TaskContext.class));
    }

    private void dispatch(WebContext webContext, Consumer<WebContext> pipelineRoot, TaskContext context) {
        try {
            context.setSubSystem(dispatcher.getClass().getSimpleName());
            dispatcher.dispatch(webContext, pipelineRoot, ctx -> {
                if (next == null) {
                    ctx.respondWith().error(HttpResponseStatus.NOT_FOUND);
                } else {
                    next.dispatch(ctx, pipelineRoot, context);
                }
            });
        } catch (Exception e) {
            if (!webContext.responseCommitted) {
                webContext.respondWith()
                          .error(HttpResponseStatus.INTERNAL_SERVER_ERROR, Exceptions.handle(WebServer.LOG, e));
            }
        }
    }

    /**
     * Pre-dispatches the given request.
     *
     * @param webContext the request to handle
     * @return <tt>true</tt> if the request was handled, <tt>false</tt> otherwise
     */
    public boolean preDispatch(WebContext webContext) {
        return preDispatch(webContext, CallContext.getCurrent().get(TaskContext.class));
    }

    private boolean preDispatch(WebContext webContext, TaskContext context) {
        try {
            context.setSubSystem(dispatcher.getClass().getSimpleName());
            if (dispatcher.preDispatch(webContext)) {
                return true;
            }
        } catch (Exception e) {
            if (!webContext.responseCommitted) {
                webContext.respondWith()
                          .error(HttpResponseStatus.INTERNAL_SERVER_ERROR, Exceptions.handle(WebServer.LOG, e));
            }
        }

        if (next == null) {
            return false;
        }

        return next.preDispatch(webContext, context);
    }
}
