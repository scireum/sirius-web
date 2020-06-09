/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.controller;

import sirius.kernel.di.std.AutoRegister;
import sirius.kernel.health.HandledException;
import sirius.web.http.WebContext;

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
 * <p>Being an async framework, the method may return without handling the request defined by the given
 * {@link WebContext}. However, one should ensure, that _some_ information is sent before a timeout occurs or the
 * idle-handler must be disabled for this request (dangerous).
 * <p>
 * Although this can be used to respond with JSON, also bear in mind, that services
 * ({@link sirius.web.services.StructuredService}) can be used to provide both, an XML and JSON interfaces at once.
 */
@AutoRegister
public interface Controller {

    /**
     * In case processing a request via a method fails (throws an exception), this method will be called.
     * <p>
     * This provides a convenient way to render a "fallback" page like a list view, if a specialized details view
     * or something thereof fails.
     *
     * @param ctx   the context containing the request
     * @param error the error which occurred
     */
    void onError(WebContext ctx, HandledException error);
}
