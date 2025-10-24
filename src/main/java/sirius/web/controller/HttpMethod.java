/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.controller;

/**
 * Enumerates the HTTP methods.
 * <p>
 * This enum is necessary for use within annotations where Netty's {@link io.netty.handler.codec.http.HttpMethod HttpMethod}
 * cannot be used. However, a conversion method {@link #toHttpMethod()} is provided.
 */
public enum HttpMethod {
    CONNECT, DELETE, GET, HEAD, OPTIONS, PATCH, POST, PUT, TRACE;

    /**
     * Converts this enum to the corresponding Netty {@link io.netty.handler.codec.http.HttpMethod HttpMethod}.
     *
     * @return the corresponding Netty HTTP method
     */
    public io.netty.handler.codec.http.HttpMethod toHttpMethod() {
        return io.netty.handler.codec.http.HttpMethod.valueOf(this.name());
    }
}
