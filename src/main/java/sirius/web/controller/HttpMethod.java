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
     * Holds all HTTP methods except OPTIONS.
     * <p>
     * OPTIONS is not part of a route's default methods and is answered centrally by the framework whether or not
     * a route opts into it (see {@link Routed#methods()}). It is therefore irrelevant to whether a route restricts
     * its HTTP methods, and is excluded from {@link #coversAllMethodsExceptOptions(HttpMethod...)}.
     */
    private static final Set<HttpMethod> METHODS_WITHOUT_OPTIONS = EnumSet.complementOf(EnumSet.of(OPTIONS));

    /**
     * Converts this enum to the corresponding Netty {@link io.netty.handler.codec.http.HttpMethod HttpMethod}.
     *
     * @return the corresponding Netty HTTP method
     */
    public io.netty.handler.codec.http.HttpMethod toHttpMethod() {
        return io.netty.handler.codec.http.HttpMethod.valueOf(this.name());
    }

    /**
     * Determines whether the given methods cover every HTTP method except the centrally handled OPTIONS, i.e. the
     * route places no meaningful restriction on the HTTP method.
     *
     * @param methods the list of methods to check
     * @return <tt>true</tt> if all methods except OPTIONS are contained, <tt>false</tt> otherwise
     */
    public static boolean coversAllMethodsExceptOptions(HttpMethod... methods) {
        if (methods == null || methods.length < METHODS_WITHOUT_OPTIONS.size()) {
            return false;
        }
        return EnumSet.of(methods[0], methods).containsAll(METHODS_WITHOUT_OPTIONS);
    }
}
