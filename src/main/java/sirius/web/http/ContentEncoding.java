/*
 * Made with all the love in the world
 * by scireum in Stuttgart, Germany
 *
 * Copyright by scireum GmbH
 * https://www.scireum.de - info@scireum.de
 */

package sirius.web.http;

import io.netty.handler.codec.http.HttpHeaderValues;
import io.netty.handler.codec.http.HttpResponseStatus;
import sirius.kernel.commons.Strings;
import sirius.kernel.health.Exceptions;
import sirius.kernel.health.HandledException;
import sirius.web.controller.Controller;

import java.io.IOException;
import java.io.InputStream;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.zip.GZIPInputStream;

/**
 * Lists the content encodings which can be accepted for request bodies (<tt>Content-Encoding</tt> header).
 * <p>
 * A service has to enable each encoding it accepts explicitly via
 * {@link sirius.web.services.PublicService#acceptEncodings()} or
 * {@link sirius.web.services.InternalService#acceptEncodings()}. Requests using any other encoding are rejected as
 * specified by RFC 7694 (HTTP Client-Initiated Content-Encoding).
 *
 * @see WebContext#decodeContent(java.util.Set)
 */
public enum ContentEncoding {

    /**
     * Represents a body compressed using gzip (RFC 1952).
     */
    GZIP("gzip", "x-gzip") {
        @Override
        public InputStream decode(InputStream inputStream) throws IOException {
            return new GZIPInputStream(inputStream, INPUT_BUFFER_SIZE);
        }
    };

    /**
     * Reads the encoded data in larger blocks than the default of 512 bytes used by {@link GZIPInputStream}.
     */
    private static final int INPUT_BUFFER_SIZE = 8 * 1024;

    /**
     * Contains the name of the encoding, followed by its aliases.
     */
    private final String[] names;

    ContentEncoding(String... names) {
        this.names = names;
    }

    /**
     * Returns the name of the encoding as used in the <tt>Content-Encoding</tt> and <tt>Accept-Encoding</tt> headers.
     *
     * @return the name of the encoding
     */
    public String getName() {
        return names[0];
    }

    /**
     * Wraps the given stream so that it provides the decoded data.
     *
     * @param inputStream the stream providing the encoded data
     * @return a stream providing the decoded data
     * @throws IOException if the data is not encoded properly
     */
    public abstract InputStream decode(InputStream inputStream) throws IOException;

    /**
     * Determines the encoding for the given content coding, as found in a <tt>Content-Encoding</tt> header.
     *
     * @param contentCoding a single content coding (like <tt>gzip</tt>), which is matched ignoring case
     * @return the matching encoding or an empty optional if the coding is unknown
     */
    public static Optional<ContentEncoding> parse(String contentCoding) {
        return Arrays.stream(values()).filter(encoding -> encoding.matches(contentCoding)).findFirst();
    }

    /**
     * Determines the encoding in which a request body was sent, as specified by RFC 7694.
     * <p>
     * The body may be sent without encoding (no <tt>Content-Encoding</tt> header or <tt>identity</tt>) or in one of
     * the given accepted encodings. Multiple (stacked) codings like <tt>gzip, gzip</tt> are rejected deliberately,
     * as each layer would multiply the effort to decode the body.
     *
     * @param contentEncodingHeaders the values of all <tt>Content-Encoding</tt> headers of the request
     * @param acceptedEncodings      the encodings accepted by the route
     * @return the encoding to decode the body with, or an empty optional if the body isn't encoded
     * @throws HandledException with <tt>415 Unsupported Media Type</tt> and an <tt>Accept-Encoding</tt> header listing
     *                          the accepted encodings (or <tt>identity</tt> if none are accepted), if the body uses
     *                          an encoding which isn't accepted
     */
    public static Optional<ContentEncoding> negotiate(List<String> contentEncodingHeaders,
                                                      Set<ContentEncoding> acceptedEncodings) {
        List<String> contentCodings = parseContentCodings(contentEncodingHeaders);

        if (contentCodings.isEmpty()) {
            return Optional.empty();
        }

        Optional<ContentEncoding> encoding = Optional.empty();

        if (contentCodings.size() == 1) {
            encoding = parse(contentCodings.getFirst()).filter(acceptedEncodings::contains);
        }

        if (encoding.isEmpty()) {
            throw Exceptions.createHandled()
                            .withDirectMessage(Strings.apply("The content encoding '%s' is not supported.",
                                                             String.join(", ", contentCodings)))
                            .hint(Controller.HTTP_STATUS, HttpResponseStatus.UNSUPPORTED_MEDIA_TYPE.code())
                            .hint(Controller.HTTP_HEADER_ACCEPT_ENCODING, formatAcceptEncoding(acceptedEncodings))
                            .handle();
        }

        return encoding;
    }

    /**
     * Returns the content codings listed in the given <tt>Content-Encoding</tt> headers, without <tt>identity</tt>.
     *
     * @param contentEncodingHeaders the values of all <tt>Content-Encoding</tt> headers of the request
     * @return the list of content codings in the order in which they were applied
     */
    private static List<String> parseContentCodings(List<String> contentEncodingHeaders) {
        return contentEncodingHeaders.stream()
                                     .flatMap(header -> Arrays.stream(header.split(",")))
                                     .map(String::trim)
                                     .filter(Strings::isFilled)
                                     .filter(Predicate.not(HttpHeaderValues.IDENTITY::contentEqualsIgnoreCase))
                                     .toList();
    }

    private static String formatAcceptEncoding(Set<ContentEncoding> acceptedEncodings) {
        if (acceptedEncodings.isEmpty()) {
            return HttpHeaderValues.IDENTITY.toString();
        }

        return acceptedEncodings.stream().map(ContentEncoding::getName).collect(Collectors.joining(", "));
    }

    private boolean matches(String contentCoding) {
        return Arrays.stream(names).anyMatch(contentCoding::equalsIgnoreCase);
    }
}
