/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

/**
 * Provides access to various template engines which generate HTML, text or PDF files. Depending on the needs,
 * a super fast static typed engine or a flexible untyped engine are available to fit most needs.
 * <p>
 * Integrates the Rythm-Engine (http://www.rythmengine.org) to render HTML. Used mainly by controllers
 * ({@link sirius.web.controller.Controller}) by calling
 * {@link sirius.web.http.Response#template(String, Object...)}
 * <p>
 * Provides a content generation framework using velocity (http://velocity.apache.org) or JavaScript to generate
 * various types of outputs. This can be done by calling {@link sirius.web.templates.Templates#generator()} and
 * using the fluent API to control the generated output.
 */
package sirius.web.templates;