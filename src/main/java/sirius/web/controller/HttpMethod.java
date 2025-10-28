/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.controller;

import java.util.EnumSet;
import java.util.Set;

/**
 * Enumerates the HTTP methods.
 * <p>
 * This enum is necessary for use within annotations where Netty's {@link io.netty.handler.codec.http.HttpMethod HttpMethod}
 * cannot be used. However, a conversion method {@link #toHttpMethod()} is provided.
 */
public enum HttpMethod {
    CONNECT, DELETE, GET, HEAD, OPTIONS, PATCH, POST, PUT, TRACE;

    /**
     * Holds all possible HTTP methods for quick reference.
     */
    private static final Set<HttpMethod> ALL_METHODS = EnumSet.allOf(HttpMethod.class);

    /**
     * Converts this enum to the corresponding Netty {@link io.netty.handler.codec.http.HttpMethod HttpMethod}.
     *
     * @return the corresponding Netty HTTP method
     */
    public io.netty.handler.codec.http.HttpMethod toHttpMethod() {
        return io.netty.handler.codec.http.HttpMethod.valueOf(this.name());
    }

    /**
     * Determines if the given array of methods contains all possible HTTP methods.
     *
     * @param methods the list of methods to check
     * @return <tt>true</tt> if all HTTP methods are contained, <tt>false</tt> otherwise
     */
    public static boolean isCompleteList(HttpMethod... methods) {
        if (methods == null || methods.length == 0 || methods.length < ALL_METHODS.size()) {
            return false;
        }
        return EnumSet.of(methods[0], methods).containsAll(ALL_METHODS);
    }
}
