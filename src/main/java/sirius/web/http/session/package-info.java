/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

/**
 * Server sided session handling.
 * <p>
 *     Provides a framework for managing and storing server sided sessions. By default all sessions are stored
 *     as conventional {@link java.util.Map} object in the heap.
 * </p>
 * <p>
 *     Alternative storage engines might be added by
 *     providing an implementation of {@link sirius.web.http.session.SessionStorage}.
 * </p>
 * <p>
 *     Lifecycle management with a two step timeout is provided by the framework. Therefore short lived sessions,
 *     like those created by a search engine crawling the site are reclaimed pretty quickly to maintain server
 *     stability. (Read more here: {@link sirius.web.http.session.ServerSession})
 *
 * </p>
 */
package sirius.web.http.session;